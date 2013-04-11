/*
 * JFBTERM -
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

#include <stdlib.h>
#include <sys/types.h>

#include "pen.h"

/* pen（リストの１要素）を初期設定 */
void tpen_init(struct TPen *p)
{
	p->prev = NULL;
	p->x = p->y = 0;
	p->fcol = 7;
	p->bcol = 0;
	p->attr = 0;
}

/* prev のメモリを解放し、リンクを切る */
void tpen_final(struct TPen *p)
{
	struct TPen *q = p->prev;
	p->prev = NULL;
	if (q != NULL) {
		tpen_final(q);
		free(q);
	}
}

/* pen のコピー
 *
 * コピーはリストの１要素のみ。
 * コピー先は１要素のみとなる。（リスト全体の再帰的なコピーは行われない）
 */
void tpen_copy(struct TPen *dst, struct TPen *src)
{
	*dst = *src;
	dst->prev = NULL;
}

/* pen の描画属性を初期状態（off）にする */
void tpen_off_all_attribute(struct TPen *p)
{
	p->bcol = 0;
	p->fcol = 7;
	p->attr = 0;
}

/* pen の描画属性にハイライト属性（明るく描画）をセットする */
void tpen_higlight(struct TPen *p)
{
	p->attr |= ATTR_HIGH;
	if (p->fcol != 0)
		p->fcol |= 8;
}

/* pen の描画属性のハイライト属性を無効にする */
void tpen_dehiglight(struct TPen *p)
{
	p->attr &= ~ATTR_HIGH;
	p->fcol &= ~8;
}

/* アンダーライン属性セット */
void tpen_underline(struct TPen *p)
{
	p->attr |= ATTR_ULINE;
	p->bcol |= 8;
}

/* アンダーライン属性無効 */
void tpen_no_underline(struct TPen *p)
{
	p->attr &= ~ATTR_ULINE;
	p->bcol &= ~8;
}

static void tpen_swp_attr(struct TPen *p)
{
	u_char swp;

	p->attr &= ~ATTR_REVERSE;

	swp = p->fcol & 0x07;

	if (p->attr & ATTR_ULINE)
		swp |= 0x08;

	p->fcol = p->bcol & 0x07;

	if ((p->attr & ATTR_HIGH) && (p->fcol != 0))
                p->fcol |= 0x08;

	p->bcol = swp;
}

/* pen の描画属性に色の反転をセット
 *
 * 備考：
 * 既に反転してるpenに、再びこの関数を使っても、元に戻ったりはしない。
 * とにかく、どの場合でも、ノーマル状態に対する反転属性がセットされるだけ。
 *
 * 余談：
 * 0x07 = ２進数で 0111
 * 0x08 = ２進数で 1000
 * つまり、これらはマスク
 */
void tpen_reverse(struct TPen *p)
{
	if ((p->attr & ATTR_REVERSE) == 0)
                tpen_swp_attr(p);
}

/* pen の描画属性の色の反転を無効
 *
 * 備考：ループはしない。どの場合でも、反転を無効にするだけ。
 */
void tpen_no_reverse(struct TPen *p)
{
	if (p->attr & ATTR_REVERSE)
                tpen_swp_attr(p);
}

/* コントロール番号に対応した、それに対応する色番号 */
static const u_char gsTPenReversTable[48] = {
	[30] = 0, [31] = 4, [32] = 2, [33] = 6,
	[34] = 1, [35] = 5, [36] = 3, [37] = 7,
	[40] = 0, [41] = 4, [42] = 2, [43] = 6,
	[44] = 1, [45] = 5, [46] = 3, [47] = 7,
};

/* pen に色をセットする */
void tpen_set_color(struct TPen *p, const u_int col)
{
	u_char t = gsTPenReversTable[col];

	switch(col) {
	case 30 ... 37:
		if (p->attr & ATTR_REVERSE) {
			if (p->attr & ATTR_ULINE)
				t |= 0x08;

			p->bcol = t;
		} else {
			if (p->attr & ATTR_HIGH)
                                t |= 0x08;

			p->fcol = t;
		}
		break;

	case 40 ... 47:
		if (p->attr & ATTR_REVERSE) {
			if (p->attr & ATTR_HIGH)
				t |= 0x08;

			p->fcol = t;
		} else {
			if (p->attr & ATTR_ULINE)
				t |= 0x08;

			p->bcol = t;
		}
		break;
	}
}
