/*
 * JFBTERM -
 * Copyright (c) 2003 Fumitoshi UKAI <ukai@debian.or.jp>
 * Copyright (C) 1999  Noritoshi MASUICHI (nmasu@ma3.justnet.ne.jp)
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY NORITOSHI MASUICHI ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL NORITOSHI MASUICHI BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <ctype.h>
#include <errno.h>
#include <unistd.h>
#include <dlfcn.h>
#include <time.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <pwd.h>
#include <pty.h>
#include <utmp.h>
#include <grp.h>

#include "term.h"
#include "vterm.h"
#include "fbcommon.h"
#include "font.h"
#include "message.h"
#include "main.h"
#include "util.h"

#include "config.h"

static int gChildProcessId = 0;

static struct TTerm gTerm;

static void tterm_wakeup_shell(struct TTerm* p, const char* tn);
static void tterm_final(struct TTerm* p);
static void tterm_set_utmp(struct TTerm* p);
static void tterm_reset_utmp(struct TTerm* p);

/* 子プロセスが終了するまで待機し、その後 vterm, term を停止し、プロセスを終了する
 */
static void sigchld(int sig)
{
	siginfo_t siginfo;
	if(waitid(P_PID, gChildProcessId, &siginfo, WEXITED))
		die("error: sigchld(), waitid()");

	tvterm_unregister_signal();
	tterm_final(&gTerm);
	exit(EXIT_SUCCESS);
}

/* term に初期状態をセットする
 * 同時に term 内部の vterm へ初期設定をセットする関数を呼び出す
 *
 * pty, ttf ともに -1
 * name は'\0'（空）で初期設定される
 */
static void tterm_init(struct TTerm* p, const char* en)
{
	p->ptyfd = -1;
	p->ttyfd = -1;
	p->name[0] = '\0';
	tcgetattr(0, &(p->ttysave));
	tvterm_init(&(p->vterm), p,
		    gFramebuffer.width / gFontsWidth,
		    gFramebuffer.height / gFontsHeight,
		    &(gApp.gCaps), en);
}

/*
 */
static void tterm_final(struct TTerm* p)
{
	tterm_reset_utmp(p);
	tvterm_final(&(p->vterm));
}

static void application_final(void)
{
	struct TTerm* p = &gTerm;
/*
	write(1, "\x1B[?25h", 6);
*/
	tcsetattr(0, TCSAFLUSH, &(p->ttysave));
	tterm_final(p);

	tfbm_close(&gFramebuffer);
	tfont_ary_final();
}

/* 使用可能な擬似端末を見つけて端末をオープンし、値をtermにセットする
 *
 * ptyfd はマスターのファイルディスクリプタ、
 * ttyfd はスレーブのファイルディスクリプタとなる
 *
 * name にはスレーブの端末名がセットされる
 *
 * スレーブの端末パラメーター、及び、ウインドウサイズは、デフォルト設定がセットされる
 */
static int tterm_open(struct TTerm* p)
{
	p->ptyfd = posix_openpt(O_RDWR);
	if (p->ptyfd == -1) {
		print_message_f("error: tterm_open(), posix_openpt()\n");
		goto err;
	}

	if (grantpt(p->ptyfd)) {
		print_message_f("error: tterm_open(), grantpt()\n");
		goto err;
	}

	if (unlockpt(p->ptyfd)) {
		print_message_f("error: tterm_open(), unlockpt()\n");
		goto err;
	}

	if (ptsname_r(p->ptyfd, p->name, TTERM_TTYFD_NAME_MAX)) {
		print_message_f("error: tterm_open(), ptsname_r()\n");
		goto err;
	}

	p->ttyfd = open(p->name, O_RDWR);
	if (p->ttyfd == -1) {
		print_message_f("error: tterm_open(), open()\n");
		goto err;
	}

#ifdef DEBUG_TERM
	print_message_f("TTerm: ptyfd[%d], ttyfd[%d], name[%s]\n", p->ptyfd, p->ttyfd, p->name);
#endif

	return 1;

err:
	return 0;
}

#define BUF_SIZE 1024
void tterm_start(const char *tn, const char *en)
{
	struct TTerm *p = &gTerm;

	struct termios ntio;

	int ret;
	struct timeval tv;
	u_char buf[BUF_SIZE+1];

	tterm_init(p, en);
	if (!tterm_open(p))
		die("Cannot get free pty-tty.\n");

	ntio = p->ttysave;
	ntio.c_lflag &= ~(ECHO|ISIG|ICANON|XCASE);
        ntio.c_iflag = 0;
        ntio.c_oflag &= ~OPOST;
        ntio.c_cc[VMIN] = 1;
        ntio.c_cc[VTIME] = 0;
	ntio.c_cflag |= CS8;
        ntio.c_line = 0;
	tcsetattr(0, TCSAFLUSH, &ntio);

	tvterm_start(&(p->vterm));
	fflush(stdout);
	gChildProcessId = fork();
	if (gChildProcessId == 0) {
	    /* child */
	    tterm_wakeup_shell(p, tn);
	    exit(1);
	} else if (gChildProcessId < 0) {
	    print_strerror("fork");
	    exit(1);
	}
	/* parent */
	tterm_set_utmp(p);
	signal(SIGCHLD, sigchld);
	atexit(application_final);

	/* not available
	 * VtInit();
	 * VtStart();
	 */
	while (1) {
		fd_set fds;
		int max = 0;
		tv.tv_sec = 0;
		tv.tv_usec = 100000;	// 100 msec
		FD_ZERO(&fds);
		FD_SET(0,&fds);
		FD_SET(p->ptyfd,&fds);
		if (p->ptyfd > max) {
			max = p->ptyfd;
		}

		ret = select(max+1, &fds, NULL, NULL, &tv);
		if (ret == 0 || (ret < 0 && errno == EINTR))
			continue;

		if (ret < 0)
			print_strerror_and_exit("select");

		if (FD_ISSET(0, &fds)) {
			ret = read(0, buf, BUF_SIZE);

			if (ret > 0)
				write(p->ptyfd, buf, ret);

		} else if (FD_ISSET(p->ptyfd,&fds)) {
			ret = read(p->ptyfd, buf, BUF_SIZE);

			if (ret > 0) {
				/* write(1, buf, ret); */
				tvterm_emulate(&(p->vterm), buf, ret);
				tvterm_refresh(&(p->vterm));
			}
		}
	}
}

static void tterm_wakeup_shell(struct TTerm* p, const char* tn)
{
	setenv("TERM", tn, 1);
	close(p->ptyfd);
	login_tty(p->ttyfd);
	tcsetattr(0, TCSANOW, &(p->ttysave));
	setgid(getgid());
	setuid(getuid());
	sleep(1); /* XXX: wait vt swtich completed? */
	execvp(gApp.gExecShell, gApp.gExecShellArgv);
	exit(1);
}

/* 文字列中の、先頭からの "?/dev/" を読み飛ばしたアドレスを返す
 * 該当しない場合は、元の文字列の先頭アドレスをそのまま返す
 *
 * 例：
 * "/dev/tty123"  => "tty123"
 * "/dev/pty/123" => "pty/123"
 *
 * "/aaa/dev/bbb" => "bbb"
 * "/aaa/bbb/ccc" => "/aaa/bbb/ccc"
 *
 * "/devian"      => "/devian"
 */
static char* skip_dev(char* s)
{
	char dev_str[] = "/dev/";
	char* p = s;

	while(*p != '\0') {
		p = strchr(p, dev_str[0]);
		if(p == NULL) {
			break;
		} else {
			if(strcmp(p, dev_str) == 0) {
				s = p + strlen(dev_str);
				break;
			}
		}

		p++;
	}

	return s;
}

/* 文字列の右側から [0-9]数字のみが連続する文字列を抜き出した、文字列の先頭アドレスを返す
 *
 * ただし、最大で４文字まで。それ以上の長さの数字が連続した場合は、以降は切り捨てる
 *
 * 該当する数字が存在しない場合はNULLを返す
 *
 * 例：
 * "/dev/tty123"    => "123"
 * "/dev/pty/123"   => "123"
 *
 * "/dev/tty12345"  => "2345"
 * "/dev/pty/12345" => "2345"
 */
static char* suffix_num4(char* s)
{
	char* ret = NULL;

	int count = 0;
	int len = strlen(s);
	while(len-->0) {
		if(isdigit(s[len])) {
			ret = &s[len];
			count++;
		} else {
			break;
		}

		if(count >= 4) {
			break;
		}
	}

	return ret;
}

static void tterm_set_utmp(struct TTerm* p)
{
#ifdef DEBUG_TERM
	print_message_f("tterm_set_utmp(): tname=[%s], suffix=[%s]\n",
			skip_dev(p->name), suffix_num4(p->name));
#endif
	struct utmp utmp;
	memset((char*)&utmp, 0, sizeof(utmp));

	char* tnum = suffix_num4(p->name);
	strncpy(utmp.ut_id, tnum, sizeof(utmp.ut_id));

	utmp.ut_type = DEAD_PROCESS;

	setutent();


	getutid(&utmp);

	utmp.ut_type = USER_PROCESS;

	utmp.ut_pid = getpid();

	char* tname = skip_dev(p->name);
	strncpy(utmp.ut_line, tname, sizeof(utmp.ut_line));

	struct passwd* pw = getpwuid(util_getuid(&vuid));
	strncpy(utmp.ut_user, pw->pw_name, sizeof(utmp.ut_user));

	time(&(utmp.ut_time));

	pututline(&utmp);

	endutent();
}

static void tterm_reset_utmp(struct TTerm* p)
{
#ifdef DEBUG_TERM
	print_message_f("tterm_reset_utmp(): tname=[%s], suffix=[%s]\n",
			skip_dev(p->name), suffix_num4(p->name));
#endif
	struct utmp utmp;
	memset((char*)&utmp, 0, sizeof(utmp));

	char* tnum = suffix_num4(p->name);
	strncpy(utmp.ut_id, tnum, sizeof(utmp.ut_id));

	utmp.ut_type = USER_PROCESS;

	setutent();


	struct utmp* utp = getutid(&utmp);

	utp->ut_type = DEAD_PROCESS;

	memset(utp->ut_user, 0, sizeof(utmp.ut_user));
	utp->ut_type = DEAD_PROCESS;

	time(&(utp->ut_time));

	pututline(utp);

	endutent();
}
