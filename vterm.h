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
#include <stdint.h>
#include <sys/types.h>
#include <stdbool.h>

#include "getcap.h"
#include "config.h"
#include "pen.h"
#include "font.h"

#include <iconv.h>

#ifndef INCLUDE_VTERM_H
#define INCLUDE_VTERM_H

#define	LEN_REPORT 9

struct TFontSpec {
	uint32_t invokedGn;	/* 呼び出さされている Gn : n = 0..3 */
	uint32_t idx;		/* 文字集合のgFont[]での位置 */
	uint32_t type; 		/* 文字集合の区分 */
	FONTSET_HALF half;	/* 文字集合のG0,G1 のどちらを使っているか */
};

#define MAX_MULTIBYTE_CHARLEN 6
struct TCodingSystem {
	uint32_t fch;
	/* iconv state */
	uint8_t *fromcode;
	uint8_t *tocode;
	iconv_t cd;
	uint8_t inbuf[MAX_MULTIBYTE_CHARLEN];
	int32_t inbuflen;
	uint8_t outbuf[MAX_MULTIBYTE_CHARLEN];

	/* saved state */
	uint32_t gSavedL;
	uint32_t gSavedR;
	uint32_t gSavedIdx[4];
	uint32_t utf8SavedIdx;
};

struct TCursor {
	uint32_t x;
	uint32_t y;
	bool on;
	bool shown;
	bool wide;
	uint32_t width;
	uint32_t height;

};

struct TVterm {
	struct TTerm *term;
	int32_t xmax;
	int32_t ymax;
	int32_t ymin;
	int32_t xcap;			/* ハード的な1行あたり文字数 */
	int32_t ycap;			/* ハード的な行数 */
	uint32_t tab;			/* TAB サイズ */

	struct TPen  pen;
	struct TPen *savedPen;
	struct TPen *savedPenSL;	/* ステータスライン用 */
	int32_t scroll;			/* スクロール行数 */
	/* -- */

	uint8_t knj1;			/* first byte of 2 byte code */
	FONTSET_HALF knj1h;
	uint32_t knj1idx;

	/* ISO-2022 対応 */
	uint32_t escSignature;
	uint32_t escGn;
	struct TFontSpec tgl;	/* 次の文字がGLのときに使う文字集合 */
	struct TFontSpec tgr;	/* 次の文字がGRのときに使う文字集合 */
	struct TFontSpec gl;	/* GL に呼び出されている文字集合 */
	struct TFontSpec gr;	/* GR に呼び出されている文字集合 */
	uint32_t gIdx[4];		/* Gn に指示されている文字集合のgFont[]での位置 */
	/* --- */
	uint32_t gDefaultL;
	uint32_t gDefaultR;
	uint32_t gDefaultIdx[4];
	/* --- */
	enum {
		SL_NONE,
		SL_ENTER,
		SL_LEAVE
	} sl;

	uint32_t utf8DefaultIdx;
	uint32_t utf8Idx;
	uint32_t utf8remain;
	uint32_t ucs2ch;

	struct TCodingSystem *otherCS;

	bool altCs;
	struct TCaps *caps;

	bool soft;
	bool wrap;
	bool ins;			/* 挿入モード */
	bool active;			/* このターミナルがアクティブ */
	bool busy;			/* ビジー状態 */
	bool sw;
	bool release;
	bool textClear;
	void (*esc)(struct TVterm *p, uint8_t ch);
	/* カーソル */
	struct TCursor cursor;

        /*  */
        struct winsize win;
	/* ESC Report Buffer */
	uint8_t report[LEN_REPORT];
	/* low level half */
	uint32_t textHead;
	uint32_t xcap4; /* 4 bytes 境界に整合した桁数(xcap + 0 ... 3) */
	uint32_t tsize; /* == xcap4 * ycap */
	/* */
	uint32_t *text; /* 1 文字当たり 4 bytes */
	uint8_t *attr;
	uint8_t *flag;
} TVterm;

void tvterm_insert_n_chars(struct TVterm *p, int32_t n);
void tvterm_delete_n_chars(struct TVterm *p, int32_t n);
void tvterm_text_scroll_down(struct TVterm *p, int32_t line);
void tvterm_text_scroll_up(struct TVterm *p, int32_t line);
void tvterm_text_move_down(struct TVterm *p, int32_t top, int32_t btm, int32_t line);
void tvterm_text_move_up(struct TVterm *p, int32_t top, int32_t btm, int32_t line);
void tvterm_text_clear_eol(struct TVterm *p, uint8_t mode);
void tvterm_text_clear_eos(struct TVterm *p, uint8_t mode);
void tvterm_wput(struct TVterm *p, uint32_t idx, uint8_t ch1, uint8_t ch2);
void tvterm_sput(struct TVterm *p, uint32_t idx, uint8_t ch);

void tvterm_uput1(struct TVterm *p, uint32_t idx, uint32_t ch);
void tvterm_uput2(struct TVterm *p, uint32_t idx, uint32_t ch);

void tvterm_text_clear_all(struct TVterm *p);

void tvterm_emulate(struct TVterm *p, const uint8_t *buff, int32_t nchars);
void tvterm_refresh(struct TVterm *p);

void tvterm_init(struct TVterm *p, struct TTerm *tp, uint32_t hx, uint32_t hy,
                 struct TCaps *caps, const uint8_t *en);
void tvterm_start(struct TVterm *p);
void tvterm_final(struct TVterm *p);

void tvterm_unregister_signal(void);
void tvterm_register_signal(struct TVterm *p);

void tvterm_show_sequence(FILE *fp, struct TCaps *cap, const uint8_t *en);

/*
 * flagBuff:
 * |      7|      6|      5|4||3|2|1|0|
 * |CLEAN_S|LATCH_2|LATCH_1| ||<----->|
 * |0=latch|  byte2|  byte1| ||   LANG|
 *
 */

#endif /* INCLUDE_VTERM_H */
