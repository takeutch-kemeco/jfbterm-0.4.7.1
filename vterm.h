/*
 * JFBTERM -
 * Copyright (c) 2003  Fumitoshi UKAI <ukai@debian.or.jp>
 * Copyright (C) 1999  Noritoshi MASUICHI (nmasu@ma3.justnet.ne.jp)
 *
 * KON2 - Kanji ON Console -
 * Copyright (C) 1992-1996 Takashi MANABE (manabe@papilio.tutics.tut.ac.jp)
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
 * THIS SOFTWARE IS PROVIDED BY NORITOSHI MASUICH AND TAKASHI MANABE ``AS IS''
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
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
#include <stdbool.h>

#include "getcap.h"
#include "config.h"
#include "pen.h"
#include "font.h"

#include <iconv.h>

#ifndef INCLUDE_VTERM_H
#define INCLUDE_VTERM_H

#define	LEN_REPORT	9

struct TCursor {
	u_int x;
	u_int y;
	bool  on;
	bool  shown;
	bool  wide;
	u_int width;
	u_int height;

};

struct TVterm {
	struct TTerm* term;
	int xmax;
	int ymax;
	int ymin;
	int xcap;			/* ハード的な1行あたり文字数 */
	int ycap;			/* ハード的な行数 */
	u_int tab;			/* TAB サイズ */
	
	struct TPen  pen;
	struct TPen* savedPen;
	struct TPen* savedPenSL;	/* ステータスライン用 */
	int scroll;			/* スクロール行数 */

	/* --- */
	enum {
		SL_NONE,
		SL_ENTER,
		SL_LEAVE
	} sl;

	u_int utf8DefaultIdx;
	u_int utf8Idx;
	u_int utf8remain;
	u_int ucs2ch;

	struct TCaps* caps;

	bool wrap;
	bool ins;			/* 挿入モード */
	bool active;			/* このターミナルがアクティブ */
	bool busy;			/* ビジー状態 */
	bool release;
	bool textClear;
	void (*esc)(struct TVterm* p, u_char ch);
	/* カーソル */
	struct TCursor cursor;

	/* ESC Report Buffer */
	char report[LEN_REPORT];
	/* low level half */
	u_int textHead;
	u_int xcap4; /* 4 bytes 境界に整合した桁数(xcap + 0 ... 3) */
	u_int tsize; /* == xcap4 * ycap */
	/* */
	u_int* text; /* 1 文字当たり 4 bytes */
	u_char* attr;
	u_char* flag;
} TVterm;

void tvterm_insert_n_chars(struct TVterm* p, int n);
void tvterm_delete_n_chars(struct TVterm* p, int n);
void tvterm_text_scroll_down(struct TVterm* p, int line);
void tvterm_text_scroll_up(struct TVterm* p, int line);
void tvterm_text_move_down(struct TVterm* p, int top, int btm, int line);
void tvterm_text_move_up(struct TVterm* p, int top, int btm, int line);
void tvterm_text_clear_eol(struct TVterm* p, u_char mode);
void tvterm_text_clear_eos(struct TVterm* p, u_char mode);
void tvterm_wput(struct TVterm* p, u_int idx, u_char ch1, u_char ch2);
void tvterm_sput(struct TVterm* p, u_int idx, u_char ch);

void tvterm_uput1(struct TVterm* p, u_int idx, u_int ch);
void tvterm_uput2(struct TVterm* p, u_int idx, u_int ch);

void tvterm_text_clear_all(struct TVterm* p);

void tvterm_emulate(struct TVterm* p, const char *buff, int nchars);
void tvterm_refresh(struct TVterm* p);

void tvterm_init(struct TVterm* p, struct TTerm* tp, u_int hx, u_int hy,
                 struct TCaps* caps, const char* en);
void tvterm_start(struct TVterm* p);
void tvterm_final(struct TVterm* p);

void tvterm_unregister_signal(void);
void tvterm_register_signal(struct TVterm* p);

void tvterm_show_sequence(FILE* fp, struct TCaps* cap, const char* en);

/*
 * flagBuff:
 * |      7|      6|      5|4||3|2|1|0|
 * |CLEAN_S|LATCH_2|LATCH_1| ||<----->|
 * |0=latch|  byte2|  byte1| ||   LANG|
 *
 */

#endif /* INCLUDE_VTERM_H */
