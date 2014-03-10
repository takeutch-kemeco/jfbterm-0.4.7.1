/*
 * JFBTERM -
 * Copyright (C) 2003  Fumitoshi UKAI <ukai@debian.or.jp>
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
 * THIS SOFTWARE IS PROVIDED BY TAKASHI MANABE ``AS IS'' AND ANY
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

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdbool.h>
#include <termios.h>
#include <malloc.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/kd.h>
#include <errno.h>

#include "config.h"
#include "util.h"
#include "term.h"
#include "vterm.h"
#include "skipagent.h"
#include "font.h"
#include "csv.h"

#include "sequence.h"
#include "message.h"

#define	LEN_REPORT 9

static void tvterm_set_default_encoding(struct TVterm* p, const char* en);
static void tvterm_esc_start(struct TVterm* p, u_char ch);
static void tvterm_esc_bracket(struct TVterm*, u_char);
static void tvterm_esc_traditional_multibyte_fix(struct TVterm* p, u_char ch);
static int tvterm_find_font_index(int fsig);
static void tvterm_esc_designate_font(struct TVterm* p, u_char ch);
static void tvterm_set_window_size(struct TVterm* p);

static void tvterm_switch_to_UTF8(struct TVterm *p);
static int tvterm_is_UTF8(struct TVterm *p);

/* TVterm の各種パラメーターの初期化 */
void tvterm_init(struct TVterm* p, struct TTerm* pt, u_int hw, u_int hh,
                 struct TCaps* caps, const char* en)
{
	p->term = pt;
	p->xcap = hw;
	p->xcap4 = (hw+7) & ~3;
	p->ycap = hh;

	tpen_init(&(p->pen));
	p->savedPen = NULL;
	p->savedPenSL = NULL;
	p->scroll = 0;
	p->wrap = false;
	p->esc = NULL;

	p->textHead = 0;

	p->utf8DefaultIdx = 0;
	p->utf8Idx = 0;
	p->utf8remain = 0;
	p->ucs2ch = 0;

	p->caps = caps;
	tvterm_set_default_encoding(p, en);
}

static int tvterm_UTF8index(const char* en)
{
	int id = 0;

	struct TCsv farg;
	tcsv_init(&farg, en);

	const char *g = tcsv_get_token(&farg);

	if(strcmp(g, "UTF-8") != 0) {
		goto FINALIZE;
	}

	if(farg.cap != 2) {
		goto FINALIZE;
	}

	g = tcsv_get_token(&farg);
	int i;
	for(i = 0; gFont[i].name != NULL; i++) {
		if(strcmp(gFont[i].name, g) == 0) {
			if(tfont_is_loaded(&gFont[i])) {
				id = i;
				goto FINALIZE;
			}
		}
	}

FINALIZE:
	tcsv_final(&farg);

	return id;
}

/* TVterm を UTF-8 モードへ変更する */
static void tvterm_switch_to_UTF8(struct TVterm* p)
{
	if(p->utf8DefaultIdx == 0) {
		const char *en = tcaps_find_entry(p->caps,
						  "encoding.",
						  "UTF-8");
		if(en != NULL) {
			p->utf8DefaultIdx = tvterm_UTF8index(en);
		}
	}
	p->utf8Idx = p->utf8DefaultIdx;
	p->utf8remain = 0;
	p->ucs2ch = 0;
}

/* TVterm が UTF-8 モードかどうかを確認する */
static int tvterm_is_UTF8(struct TVterm* p)
{
	if(p->utf8Idx != 0) {
		return 1;
	} else {
		return 0;
	}
}

/* UTF-8,<fontsetname> */
static inline void tvterm_set_default_encoding_utf8(struct TVterm* p,
						    const char* en)
{
#ifdef JFB_UTF8
	struct TCsv farg;
	tcsv_init(&farg, en);

	const char* g = tcsv_get_token(&farg);
	if(strcmp(g, "UTF-8") == 0) {
		/* UTF-8,<fontsetname> */
		p->utf8DefaultIdx = tvterm_UTF8index(en);
		tvterm_switch_to_UTF8(p);
	}

	tcsv_final(&farg);
#endif
}

void tvterm_set_default_encoding(struct TVterm* p, const char* en)
{
	tvterm_set_default_encoding_utf8(p, en);
}

void tvterm_set_default_invoke_and_designate(struct TVterm* p)
{
	p->utf8Idx = p->utf8DefaultIdx;
	p->utf8remain = 0;
	p->ucs2ch = 0;
}

void tvterm_start(struct TVterm* p)
{
	p->pen.x = 0;
	p->pen.y = 0;
	p->xmax = p->xcap;
	p->ymax = p->ycap;
	p->tab = 8;
	p->pen.fcol = 7;
	p->pen.bcol = 0;
	p->pen.attr = 0;
	p->esc = NULL;
	p->ins = false;
	p->wrap = false;
	p->active = true;

	/* ISO-2022 */
	tvterm_set_default_invoke_and_designate(p);

	p->cursor.x = 0;
	p->cursor.y = 0;
	p->cursor.on = true;
	p->cursor.shown = false;
	p->cursor.wide = false;
	p->cursor.width = gFontsWidth;
	p->cursor.height = gFontsHeight;

	p->tsize = p->xcap4 * p->ycap;
	p->text = (u_int*)calloc(p->xcap4, p->ycap * sizeof(u_int));
	p->attr = (u_char*)calloc(p->xcap4, p->ycap);
	p->flag = (u_char*)calloc(p->xcap4, p->ycap);

	ioctl(0, KDSETMODE, KD_GRAPHICS);
	/*
	 * ioctl(0, TIOCGWINSZ, &text_win);
	 * cInfo.shown = FALSE;
	 * saved = FALSE;
	 * GraphMode();
	ioctl(0, TIOCGWINSZ, p->winrect);
	 */
        /*  */
	tvterm_register_signal(p);
	tvterm_set_window_size(p);

	init_skip_agent(&skip_agent_context);
}

void tvterm_final(struct TVterm* p)
{
	close_skip_agent(&skip_agent_context);

	ioctl(0, KDSETMODE, KD_TEXT);
	tpen_final(&(p->pen));
	if(p->savedPen) {
		tpen_final(p->savedPen);
		free(p->savedPen);
		p->savedPen = NULL;
	}
	if(p->savedPenSL) {
		tpen_final(p->savedPenSL);
		free(p->savedPenSL);
		p->savedPenSL = NULL;
	}
	p->textHead = 0;
	UTIL_FREE(p->text);
	UTIL_FREE(p->attr);
	UTIL_FREE(p->flag);
}

void tvterm_push_current_pen(struct TVterm* p, bool b)
{
	struct TPen* t;
	struct TPen** base;
	base = b ? &(p->savedPen) : &(p->savedPenSL);
	t = (struct TPen*)malloc(sizeof(*t));

	if(!t) {
		return;
	}

	tpen_init(t);
	tpen_copy(t, &(p->pen));
	t->prev = *base;
	*base = t;
}

void tvterm_pop_pen_and_set_currnt_pen(struct TVterm* p, bool b)
{
	struct TPen* t;
	struct TPen** base;
	base = b ? &(p->savedPen) : &(p->savedPenSL);
	t = *base;

	if(!t) {
		return;
	}

	tpen_copy(&(p->pen), t);

	p->pen.y = limit_inner(p->pen.y, p->ymin, p->ymax - 1);

	*base = t->prev;
	free(t);
}

static inline void INSERT_N_CHARS_IF_NEEDED(struct TVterm* p, int n)
{
	if (p->ins) {
		tvterm_insert_n_chars(p, n);
	}
}

static inline void SET_WARP_FLAG_IF_NEEDED(struct TVterm* p)
{
	if (p->pen.x == p->xmax-1) {
		p->wrap = true;
	}
}

static int tvterm_put_uchar(struct TVterm* p, u_int ch)
{
	TFont *pf = &gFont[p->utf8Idx];
	u_int w;
	if(p->pen.x == p->xmax) {
		p->wrap = true;
		p->pen.x--;
	}

	if(p->wrap) {
		p->pen.x -= p->xmax - 1;

		if (p->pen.y == p->ymax - 1) {
			p->scroll++;
		} else {
			p->pen.y++;
		}

		p->wrap = false;

		return -1;
	}

	pf->conv(pf, ch, &w);

	if(pf->width == w) {
		INSERT_N_CHARS_IF_NEEDED(p, 1);
		tvterm_uput1(p, p->utf8Idx, ch);
		p->pen.x++;
	} else {
		INSERT_N_CHARS_IF_NEEDED(p, 2);
		tvterm_uput2(p, p->utf8Idx, ch);
		p->pen.x += 2;
	}

	p->utf8remain = 0;
	p->ucs2ch = 0;

	return 0;
}

int tvterm_iso_C0_set(struct TVterm* p, u_char ch)
{
	switch(ch) {
	case ISO_BS:
		if(p->pen.x) {
			p->pen.x--;
		}

		p->wrap = false;
		break;

	case ISO_HT:
		p->pen.x += p->tab - (p->pen.x % p->tab);
		p->wrap = false;
		if(p->pen.x < p->xmax) {
			break;
		}

		p->pen.x -= p->xmax;
		/* fail into next case */

	case ISO_VT:
	case ISO_FF:
		/* fail into next case */

	case ISO_LF:
		p->wrap = false;
		if(p->pen.y == p->ymax - 1) {
			p->scroll++;
		} else  {
			p->pen.y++;
		}
		break;

	case ISO_CR:
		p->pen.x = 0;
		p->wrap = false;
		break;

	case UNIVERSAL_ESC:
		p->esc = tvterm_esc_start;
		return 1;

	default:
		break;
	}

	return 0;
}

#define UTF8_CHECK_START(p) {		\
	if((p)->utf8remain != 0) {	\
		(p)->ucs2ch = 0;	\
		(p)->utf8remain = 0;	\
	}				\
}

int tvterm_put_utf8_char(struct TVterm *p, u_char ch)
{
	if(ch < 0x7F) {
		UTF8_CHECK_START(p);
		p->ucs2ch = ch;
	} else if((ch & 0xC0) == 0x80) {
		if(p->utf8remain == 0) {
			/* illegal UTF-8 sequence? */
			p->ucs2ch = ch;
		} else {
			p->ucs2ch <<= 6;
			p->ucs2ch |= (ch & 0x3F);
			p->utf8remain--;
			if(p->utf8remain > 0) {
				return p->utf8remain;
			}
		}
	} else if((ch & 0xE0) == 0xC0) {
		UTF8_CHECK_START(p);
		p->utf8remain = 1;
		p->ucs2ch = (ch & 0x1F);
		return p->utf8remain;
	} else if((ch & 0xF0) == 0xE0) {
		UTF8_CHECK_START(p);
		p->utf8remain = 2;
		p->ucs2ch = (ch & 0x0F);
		return p->utf8remain;
	} else {
		/* XXX: support UCS2 only */
	}

	int rev;
	rev = tvterm_put_uchar(p, p->ucs2ch);

	if(rev < 0) {
		p->ucs2ch >>= 6;
		if(p->ucs2ch) {
			p->utf8remain++;
		}
	}

	return rev;
}

void tvterm_emulate(struct TVterm* p, const char* buff, int nchars)
{
	while(nchars-- > 0) {
		u_char ch = *(buff++);
		if(!ch) {
			continue;
		} else if(p->esc) {
			p->esc(p, ch);
		} else if(ch < 0x20) {
			int cn = tvterm_iso_C0_set(p, ch);
			if(cn) {
				continue;
			}
		} else if(tvterm_is_UTF8(p)) {
			int rev = tvterm_put_utf8_char(p, ch);
			if(rev >= 0) {
				continue;
			} else if(rev < 0) {
				nchars -= rev;
				buff += rev;
			}
		} else if(ch == ISO_DEL) {
			/* nothing to do. */
		}

		if(p->scroll > 0) {
			tvterm_text_scroll_up(p, p->scroll);
		} else if(p->scroll < 0) {
			tvterm_text_scroll_down(p, -(p->scroll));
		}
		p->scroll = 0;
	}
}

#define ESC_ISO_GnDx(n, x) {				\
		p->esc = tvterm_esc_designate_font;	\
}

#define Fe(x) ((x)-0x40)

static void tvterm_esc_start(struct TVterm* p, u_char ch)
{
	p->esc = NULL;

	switch(ch) {
	case Fe(ISO_CSI):	/* 5/11 [ */
		p->esc = tvterm_esc_bracket;
		break;

	case ISO__MBS:		/* 2/4 $ */
		p->esc = tvterm_esc_traditional_multibyte_fix;
		break;

	case ISO_GZD4:		/* 2/8 ( */
		ESC_ISO_GnDx(0, TFONT_FT_94CHAR);
		break;

	case MULE__GZD6:	/* 2/12 , */
		ESC_ISO_GnDx(0, TFONT_FT_96CHAR);
		break;

	case ISO_G1D4:		/* 2/9 ) */
		ESC_ISO_GnDx(1, TFONT_FT_94CHAR);
		break;

	case ISO_G1D6:		/* 2/13 - */
		ESC_ISO_GnDx(1, TFONT_FT_96CHAR);
		break;

	case ISO_G2D4:		/* 2/10 * */
		ESC_ISO_GnDx(2, TFONT_FT_94CHAR);
		break;

	case ISO_G2D6:		/* 2/14 . */
		ESC_ISO_GnDx(2, TFONT_FT_96CHAR);
		break;

	case ISO_G3D4:		/* 2/11 + */
		ESC_ISO_GnDx(3, TFONT_FT_94CHAR);
		break;

	case ISO_G3D6:		/* 2/15 / */
		ESC_ISO_GnDx(3, TFONT_FT_96CHAR);
		break;

	case Fe(ISO_NEL):	/* 4/5 E */
		p->pen.x = 0;
		p->wrap = false;

	case Fe(TERM_IND): 	/* 4/4 D */
		if(p->pen.y == p->ymax - 1) {
			p->scroll++;
		} else {
			p->pen.y++;
		}
		break;

	case Fe(ISO_RI):	/* 4/13 M */
		if(p->pen.y == p->ymin) {
			p->scroll--;
		} else {
			p->pen.y--;
		}
		break;

	case ISO_RIS:		/* 6/3 c */
		p->pen.fcol = 7;
		p->pen.bcol = 0;
		p->pen.attr = 0;
		p->wrap = false;
		tvterm_set_default_invoke_and_designate(p);
		/* fail into next case */
		/* TERM_CAH: - conflicts with ISO_G2D4, no terminfo */
		p->pen.x = 0;
		p->pen.y = 0;
		p->wrap = false;
		tvterm_text_clear_all(p);
		break;

	case DEC_SC:		/* 3/7 7 */
		tvterm_push_current_pen(p, true);
		break;

	case DEC_RC:		/* 3/8 8 */
		tvterm_pop_pen_and_set_currnt_pen(p, true);
		p->wrap = false;
		break;
	}
}

void tvterm_esc_set_attr(struct TVterm* p, int col)
{
	static int acsIdx = 0;

	switch(col) {
	case 0: tpen_off_all_attribute(&(p->pen));
		break;

	case 1: tpen_higlight(&(p->pen));
		break;

	case 21: tpen_dehiglight(&(p->pen));
		break;

	case 4:	tpen_underline(&(p->pen));
		break;

	case 24: tpen_no_underline(&(p->pen));
		break;

	case 7:	tpen_reverse(&(p->pen));
		break;

	case 27: tpen_no_reverse(&(p->pen));
		break;

	case 11:	/* smacs, smpch */
		if(acsIdx == 0) {
			acsIdx = tvterm_find_font_index(0x30|TFONT_FT_94CHAR);
		}
		break;

	default:
		tpen_set_color(&(p->pen), col);
		break;
	}
}

static void tvterm_set_mode(struct TVterm* p, u_char mode, bool sw)
{
	switch(mode) {
	case 4:
		p->ins = sw;
		break;
	}
}

static void tvterm_esc_report(struct TVterm* p, u_char mode, u_short arg)
{
	p->report[0] = '\0';

	switch(mode) {
	case 'n':
		if(arg == 6) {
			int x = p->pen.x;
			x = limit_inner(x, x, p->xmax - 1);

			int y = p->pen.y;
			y = limit_inner(y, y, p->ymax - 1);

			sprintf(p->report, "\x1B[%d;%dR", y, x);
		} else if(arg == 5) {
			strcpy(p->report, "\x1B[0n\0");
		}
		break;

	case 'c':
		if(arg == 0) {
			strcpy(p->report, "\x1B[?6c\0");
		}
		break;
	}

	write(p->term->ptyfd, p->report, strlen(p->report));
}

static void tvterm_set_region(struct TVterm* p, int ymin, int ymax)
{
	/* FIX ME ! : 1999/10/30 */
	/* XXX: ESC[?1001r is used for mouse control by w3m. ukai 1999/10/27*/
	if(ymin < 0 || ymin >= p->ycap || ymin > ymax || ymax > p->ycap) {
	        /* ignore */
		return;
	}

	p->ymin = ymin;
	p->ymax = ymax;
	p->pen.x = 0;

	if(p->pen.y < p->ymin || p->pen.y > p->ymax-1) {
		p->pen.y = p->ymin - 1;
	}

	p->wrap = false;
}

void tvterm_set_window_size(struct TVterm* p)
{
	struct winsize win;

	win.ws_row = p->ymax;
	win.ws_col = p->xcap;
	win.ws_xpixel = 0;
	win.ws_ypixel = 0;
	ioctl(p->term->ttyfd, TIOCSWINSZ, &win);
}


static void tvterm_esc_status_line(struct TVterm* p, u_char mode)
{
	switch(mode) {
	case 'T':	/* To */
		if(p->sl == SL_ENTER) {
			break;
		}

		if(!p->savedPenSL) {
			tvterm_push_current_pen(p, false);
		}
		/* fail into next case */

	case 'S':	/* Show */
		if(p->sl == SL_NONE) {
			p->ymax = p->ycap - 1;
			tvterm_set_window_size(p);
		}

		if(mode == 'T') {
			p->sl = SL_ENTER;
			tvterm_set_region(p, p->ycap-1, p->ycap);
		}
		break;

	case 'F':	/* From */
		if(p->sl == SL_ENTER) {
			p->sl = SL_LEAVE;
			tvterm_set_region(p, 0, p->ycap -1);

			if(p->savedPenSL) {
				tvterm_pop_pen_and_set_currnt_pen(p, false);
			}
			p->savedPenSL = NULL;
		}
		break;

	case 'H':	/* Hide */
	case 'E':	/* Erase */
		if(p->sl == SL_NONE) {
			break;
		}

		tvterm_set_region(p, 0, p->ycap);
		tvterm_set_window_size(p);
		p->sl = SL_NONE;
		break;

	default:
		p->esc = tvterm_esc_bracket;
		tvterm_esc_bracket(p, mode);
		return;
	}

	p->wrap = false;
	p->esc = NULL;
}

#define	MAX_NARG 8

static void tvterm_esc_bracket(struct TVterm* p, u_char ch)
{
	u_char	n;
	static u_short varg[MAX_NARG], narg, question;

	if(ch >= '0' && ch <= '9') {
		varg[narg] = (varg[narg] * 10) + (ch - '0');
	} else if(ch == ';') {
		if(narg < MAX_NARG) {
			narg++;
			varg[narg] = 0;
		} else {
			p->esc = NULL;
		}
	} else {
		p->esc = NULL;
		switch(ch) {
		case 'K':
			tvterm_text_clear_eol(p, varg[0]);
			break;

		case 'J':
			tvterm_text_clear_eos(p, varg[0]);
			break;

		case ISO_CS_NO_CUU:
			p->pen.y -= varg[0] ? varg[0]: 1;
			if(p->pen.y < p->ymin) {
				p->scroll -= p->pen.y - p->ymin;
				p->pen.y = p->ymin;
			}
			break;

		case ISO_CS_NO_CUD:
			p->pen.y += varg[0] ? varg[0]: 1;
			if(p->pen.y >= p->ymax) {
				p->scroll += p->pen.y - p->ymin;
				p->pen.y = p->ymax-1;
			}
			break;

		case ISO_CS_NO_CUF:
			p->pen.x += varg[0] ? varg[0]: 1;
			p->wrap = false;
			break;

		case ISO_CS_NO_CUB:
			p->pen.x -= varg[0] ? varg[0]: 1;
			p->wrap = false;
			break;

		case 'G':
			p->pen.x = varg[0] ? varg[0] - 1: 0;
			p->wrap = false;
			break;

		case 'P':
			tvterm_delete_n_chars(p, varg[0] ? varg[0]: 1);
			break;

		case '@':
			tvterm_insert_n_chars(p, varg[0] ? varg[0]: 1);
			break;

		case 'L':
			tvterm_text_move_down(p, p->pen.y, p->ymax,
						varg[0] ? varg[0]: 1);
			break;

		case 'M':
			tvterm_text_move_up(p, p->pen.y, p->ymax,
						varg[0] ? varg[0]: 1);
			break;

		case 'H':
		case 'f':
			if(varg[1]) {
				p->pen.x = varg[1] - 1;
			} else {
				p->pen.x = 0;
			}
			p->wrap = false;

		case 'd':
			p->pen.y = varg[0] ? varg[0] - 1: 0;
			/* XXX: resize(1) specify large x,y */
			break;

		case 'm':
			for(n = 0; n <= narg; n++) {
				tvterm_esc_set_attr(p, varg[n]);
			}
			break;

		case 'r':
			n = varg[1];
			if (n == 0) {
			    n = p->ycap;
			}

			if(p->sl != SL_NONE) {
				if(n == p->ycap) {
					 n--;
				}
			}

			tvterm_set_region(p, varg[0] ? (varg[0] - 1): 0, n);
			break;

		case 'l':
			for(n = 0; n <= narg; n++) {
				tvterm_set_mode(p, varg[n], false);
			}
			break;

		case 'h':
			for(n = 0; n <= narg; n++) {
				tvterm_set_mode(p, varg[n], true);
			}
			break;

		case '?':
			p->esc = tvterm_esc_status_line;
			break;

		case 's':
			tvterm_push_current_pen(p, true);
			break;

		case 'u':
			tvterm_pop_pen_and_set_currnt_pen(p, true);
			break;

		case 'n':
		case 'c':
			if(question != true) {
				tvterm_esc_report(p, ch, varg[0]);
			}
			break;

		case 'R':
			break;
		}

		if(p->esc == NULL) {
			question = narg = varg[0] = varg[1] = 0;
		}
	}
}

static int tvterm_find_font_index(int fsig)
{
	int i;
	for(i = 0; gFont[i].fsignature; i++) {
		if(gFont[i].fsignature == fsig) {
			return i;
		}
	}

	return -1;
}

static void tvterm_esc_designate_font(struct TVterm* p, u_char ch)
{
	p->esc = NULL;
}

static void tvterm_esc_traditional_multibyte_fix(struct TVterm* p, u_char ch)
{
	switch(ch) {
	case 0x40 ... 0x42:
		tvterm_esc_designate_font(p, ch);
		break;

	default:
		tvterm_esc_start(p, ch);
		break;
	}
}


void tvterm_show_sequence(FILE* tf, struct TCaps* cap, const char* en)
{
	struct TCsv farg;
	tcsv_init(&farg, en);
	const char *g = tcsv_get_token(&farg);

	if(strcmp(g, "UTF-8") == 0) {
		fprintf(tf, "%s", "\033%G");
		goto FINALIZE;
	}

FINALIZE:
	tcsv_final(&farg);

	return;
}
