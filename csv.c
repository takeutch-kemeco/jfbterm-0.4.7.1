/*
 * JFBTERM -
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

#include <string.h>
#include "csv.h"
#include "util.h"

/* TCsv の初期設定
 * 文字列のセミコロンを \0 で置き換え、文字列が何個あるかを cap に書き出す。
 * つまりトークンに切り分ける。
 *
 * 例えば文字列が "aa,bb,cc" なら、"aa" \0 "bb" \0 "cc"となり、
 * 文字列の個数 3 が cap にセットされる。
 */
void tcsv_init(struct TCsv *p, const char *s)
{
	p->buffer = strdup(s);
	p->pnt = p->buffer;
	p->cap = 0;
	p->count = 0;

	if (p->buffer == NULL)
		return;

	p->cap = 1;
	char* cp = p->buffer;
	char* cq = p->buffer;
	while(*cp != '\0') {
		if(*cp == ',') {
			*cq = '\0';
			p->cap++;
		} else {
			*cq = *cp;
		}

		cp++;
		cq++;
	}
	*cq = '\0';
}

/* TCsv のメモリー解放 */
void tcsv_final(struct TCsv *p)
{
	UTIL_FREE(p->buffer);
}

/* TCsv からトークンを１個取り出す
 *
 * トークンを１個取り出すと、カレントシーク位置を次のトークンへと進める。
 * この関数を連続して呼び出すことで、トークンを先頭から順番に読み出せる。
 *
 * それ以上読み出すトークンが無い場合は NULL を返す
 */
const char* tcsv_get_token(struct TCsv *p)
{
	if (p->count >= p->cap)
		return NULL;

	char *ret = p->pnt;

	while (*(p->pnt) != '\0')
		p->pnt++;

	p->pnt++;
	p->count++;

	return ret;
}
