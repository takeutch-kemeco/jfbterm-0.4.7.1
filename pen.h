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

#ifndef INCLUDE_PEN_H
#define INCLUDE_PEN_H

#define ATTR_ULINE      0x80	/* under line */
#define ATTR_REVERSE    0x40	/* reverse */
#define ATTR_HIGH       0x20	/* high */

#define LATCH_S         0x0	/* single byte char */
#define LATCH_1         0x20	/* double byte char 1st byte */
#define LATCH_2         0x40	/* double byte char 2nd byte */

#define CLEAN_S         0x80
#define CODEIS_1        LATCH_1
#define CODEIS_2        LATCH_2
#define LANG_CODE       0x0F

struct TPen {
	struct TPen *prev;
	uint8_t x;
	uint8_t y;
	uint8_t bcol;
	uint8_t fcol;
	uint8_t attr;
};

void tpen_init(struct TPen *p);
void tpen_final(struct TPen *p);
void tpen_copy(struct TPen *dst, struct TPen *src);
inline void tpen_off_all_attribute(struct TPen *p);
void tpen_higlight(struct TPen *p);
void tpen_dehiglight(struct TPen *p);
void tpen_underline(struct TPen *p);
void tpen_no_underline(struct TPen *p);
void tpen_reverse(struct TPen *p);
void tpen_no_reverse(struct TPen *p);
void tpen_set_color(struct TPen *p, const uint32_t col);

#endif /* INCLUDE_PEN_H */
