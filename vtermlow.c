/*
 * JFBTERM -
 * Copyright (c) 2003 Fumitoshi UKAI <ukai@debian.or.jp>
 * Copyright (C) 1999 Noritoshi MASUICHI (nmasu@ma3.justnet.ne.jp)
 *
 * KON2 - Kanji ON Console -
 * Copyright (C) 1992, 1993 MAEDA Atusi (mad@math.keio.ac.jp)
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
 * THIS SOFTWARE IS PROVIDED BY MASUICHI NORITOSHI AND MAEDA ATUSI AND
 * TAKASHI MANABE ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
 * BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TERRENCE R.
 * LAMBERT BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
 * OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */

#define __USE_STRING_INLINES
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <unistd.h>
#include <sys/vt.h>
#include <fcntl.h>
#include <signal.h>
#include <termios.h>
#include <sys/ioctl.h>
#include <sys/kd.h>
#include <time.h>

#include "util.h"
#include "vterm.h"
#include "term.h"
#include "font.h"
#include "fbcommon.h"
#include "config.h"
#include "skipagent.h"

static void sig_leave_virtual_console(int signum);
static void sig_enter_virtual_console(int signum);

static void tvterm_text_clean_band(struct TVterm* p, u_int top, u_int btm);

/* x,y座標からテキストバッファーのインデックスへの変換 */
static inline u_int tvterm_coord_to_index(struct TVterm* p,
					  u_int x, u_int y)
{
	return (p->textHead + x + y * p->xcap4) % p->tsize;
}

static inline int IsKanji(struct TVterm* p, u_int x, u_int y)
{
	const u_int index = tvterm_coord_to_index(p, x, y);
	return p->flag[index] & CODEIS_1;
}

static inline int IsKanji2(struct TVterm* p, u_int x, u_int y)
{
	const u_int index = tvterm_coord_to_index(p, x, y);
	return p->flag[index] & CODEIS_2;
}

static inline void KanjiAdjust(struct TVterm* p, u_int* x, u_int* y)
{
	if(IsKanji2(p, *x, *y)) {
		(*x) -= 1;
	}
}

/* サイズをマイナス方向へ数える memmove()
 * サイズはbyte単位で指定する。
 *
 * 先頭アドレスを、コピーバイト分だけマイナスして渡すのと同様のことをしてる。
 * 例：
 * memmove(a - 10, b - 10, 10);
 * と同じこと
 */
static inline void brmove(void* dst, void* src, int n)
{
	memmove((void*)((char*)dst-n),(void*)((char*)src-n),n);
}

/* 配列の各byte単位で、8bit目を 0 にする*/
static inline void blatch(void* head, int n)
{
 	u_char* c = (u_char*)head;
	u_char* e = (u_char*)head + n;
	while(c < e) {
		*c++ &= 0x7F;
	}
}

/* blatch() と同様だが 4byte 単位で処理するので高速に見える
 *
 * （ただし、バイト境界のアラインがどう影響するかがよくわからないので、速いとも限らない）
 */
static inline void llatch(void *head, int n)
{
	u_int* a = (u_int*)head;
	u_int* e = (u_int*)head + (n>>2);
	while(a < e) {
		*a++ &= 0x7F7F7F7F;
	}
}

/* tvterm 内のバッファー text, attr, flag において、データ範囲を移動する。
 *
 * 実質上はコピーする。ただし、範囲が重複していても正しくコピーされる。
 */
static inline void tvterm_move(struct TVterm* p, int dst, int src, int n)
{
	memmove(p->text+dst, p->text+src, n * sizeof(u_int));
	memmove(p->attr+dst, p->attr+src, n);
	memmove(p->flag+dst, p->flag+src, n);
}

/* p - index を始点としての tvterm_move() */
static inline void tvterm_brmove(struct TVterm* p, int dst, int src, int n)
{
	brmove(p->text+dst, p->text+src, n * sizeof(u_int));
	brmove(p->attr+dst, p->attr+src, n);
	brmove(p->flag+dst, p->flag+src, n);
}

/* tvterm 内のバッファー text, attr, flag において、指定範囲をクリアーする */
static inline void tvterm_clear(struct TVterm* p, int top, int n)
{
	memset(p->text + top, 0, n * sizeof(*(p->text)));
	memset(p->flag + top, 0, n * sizeof(*(p->flag)));
	memset(p->attr + top, (p->pen.bcol << 4), n * sizeof(*(p->attr)));
}

/* 現在の pen の位置から n 文字分をカットする（デルではない）。
 * 処理はその１行のみ。（バッファー全体で処理するわけではない）
 *
 * 具体的処理：
 * 現在の pen の位置へ、”n文字先～行末”までを、まるごとスライドしてくる。
 * その後、スライドした分だけ空白になるはずの行末領域を、クリアーで埋める。
 */
void tvterm_delete_n_chars(struct TVterm* p, int n)
{
	u_int addr = tvterm_coord_to_index(p, p->pen.x, p->pen.y);
	u_int dx = p->xcap - p->pen.x - n;

	tvterm_move(p, addr, addr + n, dx); /* bmove */
	blatch(p->flag + addr, dx);

	addr = tvterm_coord_to_index(p, p->xcap-n, p->pen.y);
	tvterm_clear(p, addr, n);
}

/* 現在の pen の位置の手前に、n 文字分の空白スペースを挿入する。（上書きではなく） */
void tvterm_insert_n_chars(struct TVterm* p, int n)
{
	u_int addr = tvterm_coord_to_index(p, p->xcap - 1, p->pen.y);
	u_int dx = p->xcap - p->pen.x - n;

	tvterm_brmove(p, addr, addr - n, dx);

	addr = tvterm_coord_to_index(p, p->pen.x, p->pen.y);
	blatch(p->flag + addr + n, dx);
	tvterm_clear(p, addr, n);
}

void tvterm_set_cursor_wide(struct TVterm* p, bool b)
{
	p->cursor.wide = b;
}

static inline void tvterm_show_cursor_normal(struct TVterm* p)
{
	const u_int x = gFontsWidth  * p->cursor.x;
	const u_int y = gFontsHeight * p->cursor.y;

	u_int lx;
	if (p->cursor.wide)
		lx = p->cursor.width * 2;
	else
		lx = p->cursor.width;

	const u_int ly = p->cursor.height;

	const u_int color = 0x0F;

	gFramebuffer.cap.reverse(&gFramebuffer, x, y, lx, ly, color);
}

void tvterm_show_cursor(struct TVterm* p, bool b)
{
	if (!p->cursor.on)
		return;

	if (p->cursor.shown != b) {
		tvterm_show_cursor_normal(p);
		p->cursor.shown = b;
	}
}

static void __tvterm_refresh(void* __p)
{
	struct TVterm* p = __p;

	p->busy = true;
	if(!p->active) {
		p->busy = false;
		return;
	}

	tvterm_show_cursor(p, false);

	if(p->textClear) {
		gFramebuffer.cap.fill(&gFramebuffer,
				      0, 0,
				      gFramebuffer.width,
				      gFramebuffer.height,
				      0);
		p->textClear = false;
	}

	u_int y;
	for(y = 0; y < p->ycap; y++) {
		u_int x;
		for(x = 0; x < p->xcap; x++) {
			u_int i = tvterm_coord_to_index(p, x, y);

			u_char fg = p->flag[i];
			if(fg & CLEAN_S) {
				 continue; /* already clean */
			}

			u_char fc = p->attr[i] & 0x0F;
			u_char bc = p->attr[i] >> 4;

			u_int  chlw = p->text[i];
			u_int  lang = (chlw >> 24) & 0xFF;

			p->flag[i] |= CLEAN_S;

			TFont* pf;
			u_int w;
			if(fg & CODEIS_1) {
				w = 2;
				pf = &(gFont[lang]);

				i++;
				p->flag[i] |= CLEAN_S;
			} else {
				w = 1;
				pf = &(gFont[lang]);
			}

			/* XXX: multiwidth support (unifont) */
			u_int gw;
			const u_char* glyph = pf->conv(pf, chlw, &gw);
			gFramebuffer.cap.fill(&gFramebuffer,
					      gFontsWidth * x,
					      gFontsHeight * y,
					      gFontsWidth * w,
					      gFontsHeight,
					      bc);

			if(chlw == 0) {
				continue;
			}

			gFramebuffer.cap.overlay(&gFramebuffer,
						 gFontsWidth * x,
						 gFontsHeight * y,
						 glyph,
						 gw,
						 pf->height, pf->bytew,
						 fc);
		}
	}

	if(p->pen.x < p->xcap && p->pen.y < p->ycap) {
		/* XXX: pen position go out of screen by resize(1) for example */
		tvterm_set_cursor_wide(p, IsKanji(p,p->pen.x,p->pen.y));
		p->cursor.x = p->pen.x;
		p->cursor.y = p->pen.y;
		tvterm_show_cursor(p, true);
	}

	p->busy = false;
	if(p->release) {
                sig_leave_virtual_console(SIGUSR1);
	}
}

void tvterm_refresh(struct TVterm* p)
{
	sage_throw(__tvterm_refresh, (void*)p);
}

static struct TVterm* sig_obj = NULL;

void tvterm_unregister_signal(void)
{
        signal(SIGUSR1, SIG_DFL);
        signal(SIGUSR2, SIG_DFL);

        struct vt_mode vtm;
        vtm.mode = VT_AUTO;
        vtm.waitv = 0;
        vtm.relsig = 0;
        vtm.acqsig = 0;
        ioctl(0, VT_SETMODE, &vtm);

        ioctl(0, TIOCCONS, NULL);
}

void tvterm_register_signal(struct TVterm* p)
{
        signal(SIGUSR1, sig_leave_virtual_console);
        signal(SIGUSR2, sig_enter_virtual_console);

        struct vt_mode vtm;
        vtm.mode = VT_PROCESS;
        vtm.waitv = 0;
        vtm.relsig = SIGUSR1;
        vtm.acqsig = SIGUSR2;
        ioctl(0, VT_SETMODE, &vtm);

        sig_obj = p;
        ioctl(sig_obj->term->ttyfd, TIOCCONS, NULL);

        llatch(p->flag, p->tsize);
        p->textClear = true;
        tvterm_refresh(p);
}


static void sig_leave_virtual_console(int signum)
{

        signal(SIGUSR1, sig_leave_virtual_console);
        if(sig_obj->busy) {
                sig_obj->release = true;
                return;
        } else {
                sig_obj->release = false;
                sig_obj->active = false;
                /*
                 * SetTextMode();
                 */
                ioctl(0, VT_RELDISP, 1);
        }
}

static void sig_enter_virtual_console(int signum)
{
        signal(SIGUSR2, sig_enter_virtual_console);
        if(!sig_obj->active) {
                sig_obj->active = true;
                tvterm_register_signal(sig_obj);
                signal(SIGUSR2, sig_enter_virtual_console);
        }
}

void tvterm_wput(struct TVterm* p, u_int idx, u_char ch1, u_char ch2)
{
	const u_int a = tvterm_coord_to_index(p, p->pen.x, p->pen.y);

	p->attr[a] = p->pen.fcol | (p->pen.bcol << 4);

	p->text[a + 0] = (idx << 24) | (ch1 << 8) | ch2;
	p->text[a + 1] = 0;

	p->flag[a + 0] = LATCH_1;
	p->flag[a + 1] = LATCH_2;
}

void tvterm_sput(struct TVterm* p, u_int idx, u_char ch)
{
	const u_int a = tvterm_coord_to_index(p, p->pen.x, p->pen.y);

	p->attr[a] = p->pen.fcol | (p->pen.bcol << 4);
	p->text[a] = (idx << 24) |ch;
	p->flag[a] = LATCH_S;
}

void tvterm_uput1(struct TVterm* p, u_int idx, u_int ch)
{
	const u_int a = tvterm_coord_to_index(p, p->pen.x, p->pen.y);

	p->attr[a] = p->pen.fcol | (p->pen.bcol << 4);
	p->text[a] = (idx << 24) | ch;
	p->flag[a] = LATCH_S;
}

void tvterm_uput2(struct TVterm* p, u_int idx, u_int ch)
{
	const u_int a= tvterm_coord_to_index(p, p->pen.x, p->pen.y);

	p->attr[a] = p->pen.fcol | (p->pen.bcol << 4);

	p->text[a + 0] = (idx << 24) | ch;
	p->text[a + 1] = 0;

	p->flag[a + 0] = LATCH_1;
	p->flag[a + 1] = LATCH_2;
}

/**
	行区間 [0, TVterm::ymax) をクリアする。
**/
void tvterm_text_clear_all(struct TVterm* p)
{
	u_int y;
	for (y = 0; y < p->ymax; y++) {
		const u_int a = tvterm_coord_to_index(p, 0, y);
		tvterm_clear(p, a, p->xcap4); /* lclear */
	}
	p->textClear = true;
}

/**
	行内の指定部分をクリアする。
mode
1	行 TVterm::y カラム[0, TVterm::x) をクリアする。
2	行 TVterm::y をクリアする。
ow	行 TVterm::y カラム[TVterm::x, TVterm::xcap) をクリアする。
**/
void tvterm_text_clear_eol(struct TVterm* p, u_char mode)
{
	u_char len;
	u_char x;

	switch(mode) {
	case 1:
		x = 0;
		len = p->pen.x;
		break;

	case 2:
		x = 0;
		len = p->xcap;
		break;

	default:
		x = p->pen.x;
		len = p->xcap - p->pen.x;
		break;
	}

	const u_int a = tvterm_coord_to_index(p, x, p->pen.y);
	tvterm_clear(p, a, len);
}

/**
	スクリーン内の指定部分をクリアする。
mode
1	行 [0, TVterm::y) をクリアし、さらに
	行 TVterm::y カラム[0, TVterm::x) をクリアする。
2	スクリーン全体をクリアする。
ow	行 [TVterm::y+1, TVterm::ycap) をクリアし、さらに
	行 TVterm::y カラム[TVterm::x, TVterm::xcap) をクリアする。
**/
void tvterm_text_clear_eos(struct TVterm* p, u_char mode)
{
	u_int len;
	u_int a;

	switch(mode) {
	case 1:
		tvterm_text_clean_band(p, 0, p->pen.y);
		a = tvterm_coord_to_index(p, 0, p->pen.y);
		tvterm_clear(p, a, p->pen.x);
		break;

	case 2:
		tvterm_text_clear_all(p);
		break;

	default:
		tvterm_text_clean_band(p, p->pen.y + 1, p->ycap);
		a = tvterm_coord_to_index(p, p->pen.x, p->pen.y);
		len = p->xcap - p->pen.x;
		tvterm_clear(p, a, len);
		break;
	}
}

/**
	行区間 [top, btm) をクリアする。
**/
static void tvterm_text_clean_band(struct TVterm* p, u_int top, u_int btm)
{
	u_int y;
	for(y = top; y < btm; y ++) {
		const u_int a = tvterm_coord_to_index(p, 0, y);
		tvterm_clear(p, a, p->xcap4); /* lclear */
		/* needless to latch */
	}
}

static void __tvterm_text_scroll_down(struct TVterm *p, int line)
{
        int top = p->ymin;
        int btm = p->ymax;

	if (btm <= top + line) {
		tvterm_text_clean_band(p, top, btm);
	} else {
		u_int n;
		for (n = btm - 1; n >= top + line; n--) {
			u_int dst = tvterm_coord_to_index(p, 0, n);
			u_int src = tvterm_coord_to_index(p, 0, n - line);
			tvterm_move(p, dst, src, p->xcap4); /* lmove */
			llatch(p->flag + dst, p->xcap4);
		}
		tvterm_text_clean_band(p, top, top + line);
	}
}

void tvterm_text_scroll_down(struct TVterm* p, int line)
{
	__tvterm_text_scroll_down(p, line);
}

void tvterm_text_move_down(struct TVterm *p, int top, int btm, int line)
{
        __tvterm_text_scroll_down(p, line);
}

static void __tvterm_text_scroll_up(struct TVterm* p, int line)
{
        int top = p->ymin;
        int btm = p->ymax;

	if (btm <= top + line) {
		tvterm_text_clean_band(p, top, btm);
	} else {
		u_int n;
		for (n = top; n < btm - line; n++) {
			u_int dst = tvterm_coord_to_index(p, 0, n);
			u_int src = tvterm_coord_to_index(p, 0, n + line);
			tvterm_move(p, dst, src, p->xcap4); /* lmove */
			llatch(p->flag + dst, p->xcap4);
		}
		tvterm_text_clean_band(p, btm - line, btm);
	}
}

void tvterm_text_scroll_up(struct TVterm* p, int line)
{
	__tvterm_text_scroll_up(p, line);
}

void tvterm_text_move_up(struct TVterm* p, int top, int btm, int line)
{
        __tvterm_text_scroll_up(p, line);
}
