/*
 * JFBTERM -
 * Copyright (C) 1999 Noritoshi MASUICHI (nmasu@ma3.justnet.ne.jp)
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
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
/*
 * This code is based on the following :
 * 
 * KON2 - Kanji ON Console -
 * Copyright (C) 1992-1996 Takashi MANABE (manabe@papilio.tutics.tut.ac.jp)
 */

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdbool.h>

#include "getcap.h"

#include "util.h"
#include "message.h"

#define BUF_SIZE (1 << 10)
#define MAX_COLS (1 << 8)

/* tcapvalue の初期設定
 * リスト何もつながってない状態
 */
void tcapValue_init(struct TCapValue* p)
{
	p->next = NULL;
	p->value = NULL;
}

/* tcapvalue のメモリー解放
 * ただし、開放するのは value のみ。（thisの開放処理は行っていない）
 */
void tcapValue_final(struct TCapValue* p)
{
	util_free(p->value);
}

/* リストの最後の要素へのポインタを返す
 *
 * 要素無しの場合は NULL を返す
 */
static struct TCapValue* tcapValue_seek_last(struct TCapValue* p)
{
	if(p == NULL) {
		return NULL;
	}

	while(p->next != NULL) {
		p = p->next;
	}

	return p;
}

/* dst リストの最後の要素の次に、src 要素をセットする
 *
 * *dst が NULL（要素無し）の場合は、srcが先頭に追加される
 *
 * 注意：
 * dst へはポインタのポインタを渡す点に注意
 */
static void tcapValue_append(struct TCapValue** dst,
                             struct TCapValue* src)
{
	if(*dst == NULL) {
		*dst = src;
	} else {
		struct TCapValue* end = tcapValue_seek_last(*dst);
		end->next = src;
	}
}

/* tcapability の初期設定
 * 空の単１リスト状態
 */
void tcapability_init(struct TCapability* p)
{
	p->next = NULL;
	p->name = NULL;
	p->values = NULL;
}

/* tcapability を構成する tcapvalue リストのメモリを開放
 * ただし、開放するのは tcapsvalues リストのみ。（thisの開放処理は行ってない）
 *
 * 備考：
 * tcapvalue 自体（this）のメモリーも開放される
 */
void tcapability_del_value_all(struct TCapability* p)
{
	struct TCapValue* cur;
	struct TCapValue* next;

	cur = p->values;
	while(cur != NULL) {
		next = cur->next;
		tcapValue_final(cur);
		util_free(cur);
		cur = next;
	}
	p->values = NULL;
}

/* tcapability 単体の、内部の値関係の要素のメモリーを全て開放する
 * ただし、capability 自体は開放しない（thisの開放処理は行ってない）
 *
 * リストの鎖は開放しないので、リスト構造は維持されたまま。
 * あくまで開放するのは値関係の要素のみ。
 *
 * また、tcapability はリスト構造だが、リスト全体を再帰的に開放することは行わない。
 * あくまで単体要素のみ。
 */
void tcapability_final(struct TCapability* p)
{
	tcapability_del_value_all(p);
	util_free(p->name);
}

/* tcapability に名前をセットする
 * 元々名前が入ってた場合は領域開放されて、再設定される
 */
void tcapability_set_name(struct TCapability* p, const char* name)
{
	util_free(p->name);
	if(name != NULL) {
		p->name = strdup(name);
	}
}

/* tcapability に値文字列をセットする
 */
void tcapability_add_value(struct TCapability* p, const char* value)
{
	struct TCapValue* cp = malloc(sizeof(*cp));
	tcapValue_init(cp);
	cp->value = strdup(value);

	tcapValue_append(&(p->values), cp);
}

/* cap の開始要素である tcaps の初期設定
 * リストが空状態にセットする 
 */
void tcaps_init(struct TCaps* p)
{
	p->head = NULL;
}

/* tcaps のリストを全て開放する。
 * ただし、tcaps自体は開放しない（thisの開放処理は行っていない）
 */
void tcaps_final(struct TCaps* p)
{
	struct TCapability* cur = p->head;
	while(cur != NULL) {
		struct TCapability* next = cur->next;
		tcapability_final(cur);
		util_free(cur);
		cur = next;
	}
	p->head = NULL;
}

#if DEBUG
void tcaps_debug(struct TCaps* p)
{
	struct TCapability* np = p->head;

	while(np != NULL) {
		print_warn("%s:\n", np->name);
		struct TCapValue* vp = np->values;
		while(vp) {
			print_warn("\t%s\n", vp->value);
			vp = vp->next;
		}
		np = np->next;
	}
}
#endif

/* tcaps から名前の該当する tcapability を探す
 *
 * 見つからない場合は NULL を返す。
 * （tcapability->next の最後は NULL がセットされてるので、それを返すことになる）
 */
struct TCapability* tcaps_find(struct TCaps* p, const char *name)
{
	struct TCapability* cp = p->head;

	while(cp != NULL) {
		if((cp->name != NULL) &&
		    (strcasecmp(name, cp->name) == 0)) {
			break;
		}
		cp = cp->next;
	}
	return cp;
}

/* tcaps から名前の該当する tcapability に格納されてる tcapvalue リストの先頭の
 * 文字列を返す。
 *
 * つまり名前文字列に関連付けられた、値文字列リストの先頭文字列を返す関数。
 *
 * 見つからない場合は NULL を返す
 */
char* tcaps_find_first(struct TCaps* p, const char *name)
{
	struct TCapability* cp = tcaps_find(p, name);

	if((cp == NULL) || (cp->values == NULL)) {
		return NULL;
	} else {
		return cp->values->value;
	}
}

/* prefix 文字列と name 文字列を連結した文字列による tcaps_find_first() を行う */
char* tcaps_find_entry(struct TCaps *p,
                       const char* prefix, const char* name)
{
	char* key = malloc(strlen(prefix) + strlen(name) + 1);
	strcpy(key, prefix);
	strcat(key, name);

	char *val = tcaps_find_first(p, key);

	free(key);

	return val;
}

/* tcaps に、名前文字列に対応した値文字列をリストに追加する
 *
 * f = '=' の場合は追加ではなく、代入となる
 */
void tcaps_register_nv(struct TCaps* p,
		       const char* name, const char* value, char f)
{
	struct TCapability* cp = tcaps_find(p, name);

	if(cp == NULL) {
		cp = malloc(sizeof(*cp));
		tcapability_init(cp);
		cp->next = p->head;
		p->head = cp;
		tcapability_set_name(cp, name);
	}

	if(f == '=') {
		tcapability_del_value_all(cp);
	}

	tcapability_add_value(cp, value);
}

/* 文字 ' ' と '\t' を、文字列の左側から読み飛ばしたアドレスを返す */
char* trim_left(char* cp)
{
	while(*cp != '\0') {
		if((*cp != ' ') && (*cp != '\t')) {
			break;
		}
		cp++;
	}
	return cp;
}

/* 文字 ' ' と '\t' を、文字列の左右から読み飛ばしたアドレスを返す */
char* trim_left_right(char* cp)
{
	cp = trim_left(cp);

	char* q = cp + strlen(cp);
	q--;
	
	while(cp <= q) {
		if((*q != ' ') && (*q != '\t')) {
			*(q + 1) = '\0';
			break;
		}
		q--;
	}
	return cp;
}

/* tcaps に文字列をトークンに区切って、名前文字列に対応した、値文字列をリストに登録する
 *
 * cp 文字列のフォーマットは、
 * 代入の場合は "名前、コロン、値" として与える。
 * 追加の場合は "+名前、コロン、値" として与える。
 *
 * 例：
 * 代入の場合 "name:value"
 * 追加の場合 "+name:value"
 *
 * 何らかのエラーがあった場合は false を返す。成功は true
 */
bool tcaps_register(struct TCaps* p, const char* cp)
{
	char line[MAX_COLS];
	strncpy(line, cp, MAX_COLS);
	line[MAX_COLS-1] = '\0';

	char* q = strchr(line, ':');
	if(q == NULL) {
		return false;
	}
	
	*q = '\0';

	char* n = trim_left_right(line);
	char* v = trim_left_right(q + 1);

	if(*n == '\0') {
		return false;
	}
	
	if(*n == '+') {
		tcaps_register_nv(p, n+1, v, '+');
	} else {
		tcaps_register_nv(p, n, v, '=');
	}

	return true;
}

/* ファイルから各行を読み、tcaps へセットする
 *
 * # が含まれる行は登録しない。
 * 各行の文字列のフォーマットは、tcaps_register() のコメントを参照
 */
void tcaps_read(struct TCaps* p, const char* filename)
{
	FILE* fp = fopen(filename, "rt");
	if (fp == NULL) {
		print_strerror_and_exit(filename);
	}

	print_message("(**) : Configuration file `%s'\n", filename);

	char* q;
	int nl = 0;
	char line[MAX_COLS];
	while(fgets(line, MAX_COLS, fp) != NULL) {
		nl++;

		int len = strlen(line);
		if(len > 0) {
			line[len-1] = '\0';
		}

		q = strchr(line, '#');
		if(q != NULL) {
			*q = '\0';
		}

		q = trim_left(line);
		if(*q == '\0') {
			continue;
		}

		if(strchr(line, ':') == NULL) {
			print_message("(--) : line %d, `%s'\n", nl, line);
			continue;
		}

		if(tcaps_register(p, line) == false) {
			print_message("(--) : line %d, `%s'\n", nl, line);
		} else {
			print_message("(**) : line %d, `%s'\n", nl, line);
		}
	}
	print_message("(**) : total %d lines read.\n", nl);

	fclose(fp);
}

/* main 引数を成形して tcaps に格納していく
 * エラーがあれば画面出力して報告する
 */
void tcaps_read_args(struct TCaps* p, int argc, char** argv)
{
	char* q;

	print_warn("(**) : command line option(s)\n");

	int nl;
	for(nl = 1; nl < argc; nl++) {
		q = trim_left(argv[nl]);
		if (*q == '\0') {
			continue;
		}

		if (strlen(argv[nl]) >= MAX_COLS) {
			print_warn("(--) : arg %d, `%s'\n", nl, argv[nl]);
			continue;
		}

		if (strchr(argv[nl], ':') == NULL) {
			print_warn("(--) : arg %d, `%s'\n", nl, argv[nl]);
			continue;
		}

		if (tcaps_register(p, argv[nl]) == false) {
			print_warn("(--) : arg %d, `%s'\n", nl, argv[nl]);
		}
	}

	print_warn("(**) : total %d args read.\n", nl);
}
