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

#if 0

static int	saveTime, saverCount;
static bool	saved;
static volatile bool	busy;		 /* TRUE iff updating screen */
static volatile bool	release;	 /* delayed VC switch flag */

static void ShowCursor(struct cursorInfo *, bool);

#endif

static void sig_leave_virtual_console(int signum);
static void sig_enter_virtual_console(int signum);

static void tvterm_text_clean_band(TVterm* p, u_int top, u_int btm);

/* おそらくx,y座標からテキストバッファーのインデックスへの変換 */
static inline u_int tvterm_coord_to_index(TVterm* p, u_int x, u_int y)
{
	return (p->textHead + x + y * p->xcap4) % p->tsize;
}

static inline int IsKanji(TVterm* p, u_int x, u_int y)
{
	return p->flag[tvterm_coord_to_index(p, x, y)] & CODEIS_1;
}

static inline int IsKanji2(TVterm* p, u_int x, u_int y)
{
	return p->flag[tvterm_coord_to_index(p, x, y)] & CODEIS_2;
}

static inline void KanjiAdjust(TVterm* p, u_int *x, u_int *y)
{
	if (IsKanji2(p, *x, *y)) {
		--*x;
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
		*c++ &= 0x7f;
	}
}

/* blatch() と同様だが 4byte 単位で処理するので高速に見える
 * 
 * （ただし、バイト境界のアラインがどう影響するかがよくわからないので、速いとも限らない）
 */
static inline void llatch(void *head, int n)
{
	u_long* a = (u_long*)head;
	u_long* e = (u_long*)head + (n>>2);
	while(a < e) {
		*a++ &= 0x7f7f7f7f;
	}
}

/* tvterm 内のバッファー text, attr, flag において、データ範囲を移動する。
 *
 * 実質上はコピーする。ただし、範囲が重複していても正しくコピーされる。
 */
static inline void tvterm_move(TVterm* p, int dst, int src, int n)
{
	memmove(p->text+dst, p->text+src, n*sizeof(u_int));
	memmove(p->attr+dst, p->attr+src, n);
	memmove(p->flag+dst, p->flag+src, n);
}

/* p - index を始点としての tvterm_move() */
static inline void tvterm_brmove(TVterm* p, int dst, int src, int n)
{
	brmove(p->text+dst, p->text+src, n*sizeof(u_int));
	brmove(p->attr+dst, p->attr+src, n);
	brmove(p->flag+dst, p->flag+src, n);
}

/* tvterm 内のバッファー text, attr, flag において、指定範囲をクリアーする */
static inline void tvterm_clear(TVterm* p, int top, int n)
{
	bzero(p->text+top, n*sizeof(u_int));
	bzero(p->flag+top, n);
	memset(p->attr+top, (p->pen.bcol<<4), n);
}

/* 現在の pen の位置から n 文字分をカットする（デルではない）。
 * 処理はその１行のみ。（バッファー全体で処理するわけではない）
 *
 * 具体的処理：
 * 現在の pen の位置へ、”n文字先～行末”までを、まるごとスライドしてくる。
 * その後、スライドした分だけ空白になるはずの行末領域を、クリアーで埋める。
 */
void tvterm_delete_n_chars(TVterm* p, int n)
{
	u_int addr;
	u_int dx;
	
	addr = tvterm_coord_to_index(p, p->pen.x, p->pen.y);
	dx = p->xcap - p->pen.x - n;
	
	tvterm_move(p, addr, addr+n, dx); /* bmove */
	blatch(p->flag+addr, dx);
	
	addr = tvterm_coord_to_index(p, p->xcap-n, p->pen.y);
	tvterm_clear(p, addr, n);
}

/* 現在の pen の位置の手前に、n 文字分の空白スペースを挿入する。（上書きではなく） */
void tvterm_insert_n_chars(TVterm* p, int n)
{
	u_int addr;
	u_int dx;
	
	addr = tvterm_coord_to_index(p, p->xcap-1, p->pen.y);
	dx = p->xcap - p->pen.x - n;
	tvterm_brmove(p, addr, addr-n, dx);
	
	addr = tvterm_coord_to_index(p, p->pen.x, p->pen.y);
	blatch(p->flag+addr+n, dx);
	tvterm_clear(p, addr, n);
}

#if 0 /* ハードウエアスクロールする際に使う。(現在未使用) */
static void tvterm_scroll_up_n_lines(TVterm* p, int n)
{
	int	h;
	int	t;

	h = p->textHead;
	textHead += n * p->xcap4;
	if (p->textHead > p->tsize) {
		p->textHead -= p->tsize;
		n = p->tsize - h;
		if (p->textHead) {
			tvterm_clear(p, 0, p->textHead); /* lclear */
		}
	} else  {
		n = p->textHead - h;
	}
	tvterm_clear(p, h, n); /* lclear */
}

/* n 行だけダウンスクロールする。
 * （ダウンスクロール　＝　↓カーソルを押された時の動き）
 *
 * textHead 以下に空きスペースができるはずなので、そこをクリアーする処理がメイン。
 */
static void tvterm_scroll_down_n_lines(TVterm* p, int n)
{
	int	h;
	int	t;

	h = p->textHead;
	p->textHead -= n * p->xcap4;
	if (p->textHead < 0) {
		p->textHead += p->tsize;
		if (h) {
			tvterm_clear(p, 0, h); /* lclear */
		}
		n = p->tsize - p->textHead;
	} else {
		n = h - p->textHead;
	}
	tvterm_clear(p, h, n); /* lclear */
}
#endif

void tvterm_set_cursor_wide(TVterm* p, bool b)
{
	p->cursor.wide = b;
}

void tvterm_show_cursor(TVterm* p, bool b)
{
	if (!p->cursor.on) {
		return;
	}
	if (p->cursor.shown != b) {
#ifdef JFB_REVERSEVIDEO
/* -- kei --
	On this implementation, JFB_REVERSEVIDEO is effective on
	only 2bpp machine.
	On reverse video mode, cursor should be 0x00.
	On non reverse video mode, cursor should be 0xf.
	Yes, I know following is little bit dirty code, but
	temporal fix...
*/
		if (gFramebuffer.cap.bitsPerPixel == 2) {
			gFramebuffer.cap.reverse(&gFramebuffer,
				gFontsWidth * p->cursor.x,
				gFontsHeight * p->cursor.y,
				p->cursor.width + (p->cursor.wide ? p->cursor.width : 0),
				p->cursor.height, 0x0);
		} else {
			gFramebuffer.cap.reverse(&gFramebuffer,
				gFontsWidth * p->cursor.x,
				gFontsHeight * p->cursor.y,
				p->cursor.width + (p->cursor.wide ? p->cursor.width : 0),
				p->cursor.height, 0xf);
		}
#else
		gFramebuffer.cap.reverse(&gFramebuffer,
			gFontsWidth * p->cursor.x,
			gFontsHeight * p->cursor.y,
			p->cursor.width + (p->cursor.wide ? p->cursor.width : 0),
			p->cursor.height, 0xf);
#endif
		p->cursor.shown = b;
	}
}

static void __tvterm_refresh(void* __p)
{
	TVterm* p = __p;

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

void tvterm_refresh(TVterm* p)
{
	sage_throw(__tvterm_refresh, (void*)p);
}

/*---------------------------------------------------------------------------*/

static TVterm* sig_obj = NULL;

void tvterm_unregister_signal(void)
{
        struct vt_mode vtm;

        signal(SIGUSR1, SIG_DFL);
        signal(SIGUSR2, SIG_DFL);

        vtm.mode = VT_AUTO;
        vtm.waitv = 0;
        vtm.relsig = 0;
        vtm.acqsig = 0;
        ioctl(0, VT_SETMODE, &vtm);

        ioctl(0, TIOCCONS, NULL);
}

void tvterm_register_signal(TVterm* p)
{
        struct vt_mode vtm;

        sig_obj = p;

        signal(SIGUSR1, sig_leave_virtual_console);
        signal(SIGUSR2, sig_enter_virtual_console);

        vtm.mode = VT_PROCESS;
        vtm.waitv = 0;
        vtm.relsig = SIGUSR1;
        vtm.acqsig = SIGUSR2;
        ioctl(0, VT_SETMODE, &vtm);

        ioctl(sig_obj->term->ttyfd, TIOCCONS, NULL);

        llatch(p->flag, p->tsize);
        p->textClear = true;
        tvterm_refresh(p);
}


static void     sig_leave_virtual_console(int signum)
{

        signal(SIGUSR1, sig_leave_virtual_console);
        if (sig_obj->busy) {
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

static void     sig_enter_virtual_console(int signum)
{
        signal(SIGUSR2, sig_enter_virtual_console);
        if (!sig_obj->active) {
                sig_obj->active = true;
                tvterm_register_signal(sig_obj);
                signal(SIGUSR2, sig_enter_virtual_console);
        }
}

void tvterm_wput(TVterm* p, u_int idx, u_char ch1, u_char ch2)
{
	u_int a = tvterm_coord_to_index(p, p->pen.x, p->pen.y);

	p->attr[a] = p->pen.fcol | (p->pen.bcol << 4);
	p->text[a] = (idx << 24) | (ch1 << 8) | ch2;
	p->text[a+1] = 0;
	p->flag[a] = LATCH_1;
	p->flag[a+1] = LATCH_2;
}

void tvterm_sput(TVterm* p, u_int idx, u_char ch)
{
	u_int a = tvterm_coord_to_index(p, p->pen.x, p->pen.y);

	p->attr[a] = p->pen.fcol | (p->pen.bcol << 4);
	p->text[a] = (idx << 24) |ch;
	p->flag[a] = LATCH_S;
}

#ifdef JFB_UTF8
void tvterm_uput1(TVterm* p, u_int idx, u_int ch)
{
	u_int a = tvterm_coord_to_index(p, p->pen.x, p->pen.y);

	p->attr[a] = p->pen.fcol | (p->pen.bcol << 4);
	p->text[a] = (idx << 24) | ch;
	p->flag[a] = LATCH_S;
}

void tvterm_uput2(TVterm* p, u_int idx, u_int ch)
{
	u_int a= tvterm_coord_to_index(p, p->pen.x, p->pen.y);

	p->attr[a] = p->pen.fcol | (p->pen.bcol << 4);
	p->text[a] = (idx << 24) | ch;
	p->text[a+1] = 0;
	p->flag[a] = LATCH_1;
	p->flag[a+1] = LATCH_2;
}
#endif

/**
	行区間 [0, TVterm::ymax) をクリアする。
**/
void tvterm_text_clear_all(TVterm* p)
{
	u_int y;
	u_int a;

	for (y = 0; y < p->ymax; y++) {
		a = tvterm_coord_to_index(p, 0, y);
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
void tvterm_text_clear_eol(TVterm* p, u_char mode)
{
	u_int a;
	u_char len;
	u_char x = 0;
	
	switch(mode) {
	case 1:
		len = p->pen.x;
		break;
	case 2:
		len = p->xcap;
		break;
	default:
		x = p->pen.x;
		len = p->xcap - p->pen.x;
		break;
	}
	a = tvterm_coord_to_index(p, x, p->pen.y);
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
void tvterm_text_clear_eos(TVterm* p, u_char mode)
{
	u_int	a;
	u_int	len;
	
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
static void tvterm_text_clean_band(TVterm* p, u_int top, u_int btm)
{
	u_int	y;
	u_int	a;
	
	for (y = top; y < btm; y ++) {
		a = tvterm_coord_to_index(p, 0, y);
		tvterm_clear(p, a, p->xcap4); /* lclear */
		/* needless to latch */
	}
}

/**
**/
void tvterm_text_scroll_down(TVterm* p, int line)
{
	tvterm_text_move_down(p, p->ymin, p->ymax, line);
}

void tvterm_text_move_down(TVterm* p, int top, int btm, int line)
{
	u_int	n;
	u_int	src;
	u_int	dst;

	if (btm <= top + line) {
		tvterm_text_clean_band(p, top, btm);
	} else {
		for (n = btm-1; n >= top + line; n --) {
			dst = tvterm_coord_to_index(p, 0, n);
			src = tvterm_coord_to_index(p, 0, n-line);
			tvterm_move(p, dst, src, p->xcap4); /* lmove */
			llatch(p->flag + dst, p->xcap4);
		}
		tvterm_text_clean_band(p, top, top + line);
	}
}

/**
**/
void tvterm_text_scroll_up(TVterm* p, int line)
{
	tvterm_text_move_up(p, p->ymin, p->ymax, line);
}

void tvterm_text_move_up(TVterm* p, int top, int btm, int line)
{
	u_int	n;
	u_int	src;
	u_int	dst;
	
	if (btm <= top + line) {
		tvterm_text_clean_band(p, top, btm);
	} else {
		for (n = top; n < btm - line; n ++) {
			dst = tvterm_coord_to_index(p, 0, n);
			src = tvterm_coord_to_index(p, 0, n+line);
			tvterm_move(p, dst, src, p->xcap4); /* lmove */
			llatch(p->flag + dst, p->xcap4);
		}
		tvterm_text_clean_band(p, btm - line, btm);
	}
}

void	tvterm_text_reverse(TVterm* p,int fx, int fy, int tx, int ty)
{
	u_int	from, to, y, swp, xx, x;
	u_char	fc, bc, fc2, bc2;
	
	KanjiAdjust(p, &fx, &fy);
	KanjiAdjust(p, &tx, &ty);
	if (fy > ty) {
		swp = fy;
		fy = ty;
		ty = swp;
		swp = fx;
		fx = tx;
		tx = swp;
	} else if (fy == ty && fx > tx) {
		swp = fx;
		fx = tx;
		tx = swp;
	}
	for (xx = p->xcap, y = fy; y <= ty; y ++) {
		if (y == ty) xx = tx;
		from = tvterm_coord_to_index(p, fx, y);
		to = tvterm_coord_to_index(p, xx, y);
		if (p->flag[from] & CODEIS_2)
			/* 2nd byte of kanji */
			from--;
		for (x = from; x <= to; x ++) {
			if (!p->text[x]) continue;
			fc = p->attr[x];
			bc = fc >> 4;
			bc2 = (bc & 8) | (fc & 7);
			fc2 = (fc & 8) | (bc & 7);
			p->attr[x] = fc2 | (bc2 << 4);
			p->flag[x] &= ~CLEAN_S;
		}
		fx = 0;
	}
}

#if 0
/* Cursor related routines. */

static void	ToggleCursor(struct cursorInfo *c)
{
	c->count = 0;
	if (con.text_mode)
	return;
	c->shown = ! c->shown;
	vInfo.cursor(c);
}

static void	ShowCursor(struct cursorInfo *c, bool show)
{
	if (!con.active || !c->sw)
	return;
	if (c->shown != show)
	ToggleCursor(c);
}

static void	SaveScreen(bool save)
{
	if (saved != save) {
	saved = save;
	vInfo.screen_saver(save);
	}
	saverCount = 0;
}
#endif

#if 0
/* Called when some action was over, or every 1/10 sec when idle. */

void	PollCursor(bool wakeup)
{
	if (!con.active)
	return;
	if (wakeup) {
	SaveScreen(false);
	ShowCursor(&cInfo, true);
	return;
	}
	/* Idle. */
	if (saved)
	return;
	if ((saveTime > 0) && (++saverCount == saveTime)) {
	ShowCursor(&cInfo, false);
	SaveScreen(true);
	return;
	}
	if ((cInfo.interval > 0) && (++cInfo.count == cInfo.interval)) {
	ToggleCursor(&cInfo);
	}
}

#endif

#if 0
/* Beep routines. */

#define	COUNTER_ADDR	0x61

static int	beepCount;

static int	ConfigBeep(const char *confstr)
{
	beepCount = atoi(confstr) * 10000;
	if (beepCount > 0)
	ioperm(COUNTER_ADDR, 1, true);
	return SUCCESS;
}

void	Beep(void)
{
	if (!con.active || beepCount <= 0) return;
	PortOutb(PortInb(COUNTER_ADDR)|3, COUNTER_ADDR);
	usleep(beepCount);
	PortOutb(PortInb(COUNTER_ADDR)&0xFC, COUNTER_ADDR);
}

static int	ConfigInterval(const char *confstr)
{
	cInfo.interval = atoi(confstr);
	return SUCCESS;
}

static int	ConfigSaver(const char *confstr)
{
	saveTime = atoi(confstr) * 600; /* convert unit from minitue to 1/10 sec */
	return SUCCESS;
}
#endif

