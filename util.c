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

struct VirtualUID vuid;

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
	vuid.real_uid = getuid();
	vuid.effective_uid = geteuid();

	util_privilege_off(&vuid);
}

/* ファイルをeffectiveユーザー権限で開く */
int util_privilege_open(char *pathname, int flags)
{
	util_privilege_on(&vuid);
	int fd = open(pathname, flags);
	util_privilege_off(&vuid);

	return fd;
}

