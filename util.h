/*
 * JFBTERM -
 * Copyright (c) 2003 Fumitoshi UKAI <ukai@debian.or.jp>
 * Copyright (c) 1999 Noritoshi Masuichi (nmasu@ma3.justnet.ne.jp)
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *	notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *	notice, this list of conditions and the following disclaimer in the
 *	documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY NORITOSHI MASUICHI ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE TERRENCE R. LAMBERT BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 */

#include <sys/types.h>
#include <stdlib.h>

#ifndef INCLUDE_UTIL_H
#define INCLUDE_UTIL_H

void util_privilege_init(void);
void util_privilege_on(void);
void util_privilege_off(void);
int util_privilege_open(char *pathname, int flags);
int util_privilege_ioperm(u_long from, u_int num, int turn_on);
uid_t util_getuid();
void util_privilege_drop();
int util_search_string(const char* s, const char** array);
void util_swap(u_int* a, u_int* b);

#define util_free(p) {free(p); (p) = NULL;}

#define UTIL_SWAP(a, b) {	\
	typeof(a) tmp = a;	\
	a = b;			\
	b = tmp;		\
}

#endif /* INCLUDE_UTIL_H */
