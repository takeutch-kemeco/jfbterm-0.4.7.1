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
#include <utmpx.h>
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

	tterm_init(p, en);
	if (!tterm_open(p))
		die("Cannot get free pty-tty.\n");

	struct termios ntio = p->ttysave;
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
	atexit(application_final);

	gChildProcessId = fork();
	if (gChildProcessId == 0) {
		/* child */
		tterm_wakeup_shell(p, tn);
		exit(EXIT_SUCCESS);
	} else if (gChildProcessId == -1) {
		print_strerror("fork");
		exit(EXIT_FAILURE);
	}
	/* parent */
	tterm_set_utmp(p);
	signal(SIGCHLD, sigchld);

	u_char *buf = calloc(BUF_SIZE + 1, sizeof(*buf));

	/* not available
	 * VtInit();
	 * VtStart();
	 */
	while (1) {
		fd_set fds;
		int max = 0;
		FD_ZERO(&fds);
		FD_SET(0,&fds);
		FD_SET(p->ptyfd,&fds);
		if (p->ptyfd > max)
			max = p->ptyfd;

		const int ret = pselect(max + 1, &fds, NULL, NULL, NULL, NULL);
		if (ret < 0) {
			if (errno == EINTR)
				continue;
			else
				die("error: tterm_start(), pselect()\n");
		}

		if (FD_ISSET(0, &fds)) {
			const int len = read(0, buf, BUF_SIZE);

			if (len > 0)
				write(p->ptyfd, buf, len);

		} else if (FD_ISSET(p->ptyfd, &fds)) {
			const int len = read(p->ptyfd, buf, BUF_SIZE);

			if (len > 0) {
				tvterm_emulate(&(p->vterm), buf, len);
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
	execvp(gApp.gExecShell, gApp.gExecShellArgv);
	exit(EXIT_FAILURE);
}

static void tterm_set_utmp(struct TTerm *p)
{
#ifdef DEBUG_TERM
	print_message_f("tterm_set_utmp(): tname=[%s], suffix=[%s]\n",
			p->name, p->name);
#endif
	struct utmpx *t = calloc(1, sizeof(*t));
	if (t == NULL)
		die("error: tterm_set_utmp(), calloc()");

	strncpy(t->ut_id, p->name, sizeof(t->ut_id));
	t->ut_type = DEAD_PROCESS;
	setutxent();

	if (getutxid(t) == NULL)
		die("error: tterm_set_utmp(), getutxid()");

	t->ut_type = USER_PROCESS;
	t->ut_pid = getpid();
	strncpy(t->ut_line, p->name, sizeof(t->ut_line));
	struct passwd *pw = getpwuid(util_getuid(&vuid));
	strncpy(t->ut_user, pw->pw_name, sizeof(t->ut_user));

	struct timeval tv;
	struct timezone tz;
	if (gettimeofday(&tv, &tz))
		die("error: tterm_set_utmp(), gettimeofday()");

	t->ut_tv.tv_sec = tv.tv_sec;
	t->ut_tv.tv_usec = tv.tv_usec;

	if (pututxline(t) == NULL)
		die("error: tterm_set_utmp(), pututxline()");

	endutxent();
}

static void tterm_reset_utmp(struct TTerm* p)
{
#ifdef DEBUG_TERM
	print_message_f("tterm_reset_utmp(): tname=[%s], suffix=[%s]\n",
			p->name, p->name);
#endif
	struct utmpx *t = calloc(1, sizeof(*t));
	if (t == NULL)
		die("error: tterm_reset_utmp(), calloc()");

	strncpy(t->ut_id, p->name, sizeof(t->ut_id));
	t->ut_type = USER_PROCESS;
	setutxent();

	struct utmpx *tp = getutxid(t);
	if (tp == NULL)
		die("error: tterm_reset_utmp(), getutxid()");

	tp->ut_type = DEAD_PROCESS;
	memset(tp->ut_user, 0, sizeof(t->ut_user));
	tp->ut_type = DEAD_PROCESS;

	struct timeval tv;
	struct timezone tz;
	if (gettimeofday(&tv, &tz))
		die("error: tterm_set_utmp(), gettimeofday()");

	tp->ut_tv.tv_sec = tv.tv_sec;
	tp->ut_tv.tv_usec = tv.tv_usec;

	if (pututxline(tp) == NULL)
		die("error: tterm_reset_utmp(), pututline()");

	endutxent();
}
