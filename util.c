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

#include "config.h"

#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/io.h>
#include <fcntl.h>

#include "util.h"

static uid_t effective_uid;
static uid_t real_uid;

/* ユーザーIDを、実(real)ユーザーと実効(effective)ユーザーとで逆に設定しなおす
 *
 * 余談：
 * effective ユーザーIDが、実際の効果を発揮するユーザーIDで
 * real ユーザーIDは、（アプリなどの）プロセスを起動した時点でのユーザーID
 *
 * プロセスが読み書きする際の、実際に効果がある権限としては effective の権限が用いられる。
 *
 * たとえば effective が非ルートユーザーIDならば、たとえ real がルートユーザーIDだとしても
 * プロセスが読み書きする際の権限は非ルートユーザーまで。
 *
 * もしも effective の権限自体をルートユーザーへと書き換えれば、
 * プロセスが読み書きする際の権限もルートユーザーとなる。が、その書き変えのためには real が
 * ルートユーザーでなければならない。
 * real はそのため（effectiveの権限を書き換えても良いかの判断）に用いられるだけのもの。
 *
 * あくまで実際のアクセス権限は、effective のユーザーIDによって決まる。
 */
void util_privilege_init(void)
{
	real_uid = getuid();
	effective_uid = geteuid();

	util_privilege_off();
}

/* realユーザーIDと、effectiveユーザーIDを、本来の元の状態に設定する */
void util_privilege_on(void)
{
	setreuid(real_uid, effective_uid);
}

/* realユーザーIDと、effectiveユーザーIDを、逆転した状態に設定する */
void util_privilege_off(void)
{
	setreuid(effective_uid, real_uid);
}

/* ファイルをeffectiveユーザー権限で開く */
int util_privilege_open(char *pathname, int flags)
{
	util_privilege_on();
	int fd = open(pathname, flags);
	util_privilege_off();

	return fd;
}

#ifdef HAVE_IOPERM
/* ポートの入出力許可をeffectiveユーザー権限で得る */
int util_privilege_ioperm(u_int from, u_int num, int turn_on)
{
	util_privilege_on();
	int r = ioperm(from, num, turn_on);
	util_privilege_off();

	return r;
}
#else
inline int util_privilege_ioperm(u_int from, u_int num, int turn_on)
{
	return -1
}
#endif

/* 現在の setreuid() された設定状態に関わらず、
 * システムが現在、本来のrealユーザーIDとして考えてるIDを返す。
 *
 * 本来のrealユーザーID = util_privilege_on() した場合のrealユーザーID
 */
uid_t util_getuid(void)
{
	return real_uid;
}

/* real, effectiveどちらのユーザーIDも、
 * realユーザーIDを用いる状態に設定する
 */
void util_privilege_drop()
{
	setreuid(real_uid, real_uid);
}

/* 複数の文字列による配列 array の各文字列と、文字列 s を比較し、
 * 一致したインデックスを返す。
 *
 * array 配列の最後は Null ポインターを格納してなければならない。
 *
 * 一致しなかった場合は -1 を返す。
 */
int util_search_string(const char *s, const char **array)
{
	int i;
	for (i = 0; array[i]; i++)
		if (strcmp(array[i], s) == 0)
			return i;

	return -1;
}

/* 文字列 s が " または ' で前後を囲まれてた場合、それを取り除く。
 *
 * "ABC" -> ABC
 * or
 * 'ABC' -> ABC
 */
char* remove_quote(char* s)
{
        if (s == NULL)
                return s;

        const size_t last = strlen(s) - 1;
        if ((s[0] == '"'  && s[last] == '"') || (s[0] == '\'' && s[last] == '\'')) {
                s[last] = '\0';
                s++;
	}

        return s;
}
