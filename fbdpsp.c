/*
 * JFBTERM -
 * Copyright (C) 1999  Noritoshi MASUICHI (nmasu@ma3.justnet.ne.jp)
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY NORITOSHI MASUICHI ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL NORITOSHI MASUICHI BE LIABLE
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
#include <string.h>
#include <endian.h>
#include <linux/fb.h>

#include "fbdpsp.h"
#include "fbcommon.h"

#ifndef min
#  define min(a, b)   (((a)>(b))?(b):(a))
#endif

#ifdef JFB_32BPP

/* xとyをスクリーンの回転設定に合わせて、右か左に回転移動。
 * CW（クロックワイズ）が、首を時計回りに傾けて見るのに適した状態。
 * CCW（カウンターCW）が、首を反時計回りに傾けて見るのに適した状態。
 */
static void tfbm_rot_xy_32bpp_packed(u_int *real_x, u_int *real_y,
				     const u_int x, const u_int y,
			             const u_int w, const u_int h)
{
	switch (tfbm_scr_rot_flag) {
	case TFBM_SCR_ROT_FLAG_CCW:
		*real_x = y;
		*real_y = (w - 1) - x;
		break;

	case TFBM_SCR_ROT_FLAG_CW:
		*real_x = (h - 1) - y;
		*real_y = x;
		break;

	default:
		*real_x = x;
		*real_y = y;
	}
}

static u_int* tfbm_seek_pix_adrs_32bpp_packed(TFrameBufferMemory *p,
					      const u_int x, const u_int y)
{
	u_int real_x;
	u_int real_y;
	tfbm_rot_xy_32bpp_packed(&real_x, &real_y, x, y, p->width, p->height);

	return (u_int*)(p->smem + (real_y * p->bytePerLine) + (real_x * 4));
}

static void tfbm_set_pixel_32bpp_packed(TFrameBufferMemory *p,
                                        const u_int x, const u_int y, const u_int icol)
{
	if ((x >= 0 && x < p->width) && (y >= 0 && y < p->height)) {
		u_int* d = tfbm_seek_pix_adrs_32bpp_packed(p, x, y);
		*d = icol;
	}
}

static void tfbm_xor_pixel_32bpp_packed(TFrameBufferMemory *p,
					const u_int x, const u_int y, const u_int icol)
{
	if ((x >= 0 && x < p->width) && (y >= 0 && y < p->height)) {
		u_int* d = tfbm_seek_pix_adrs_32bpp_packed(p, x, y);
		*d ^= icol;
	}
}

void tfbm_fill_rect_32bpp_packed(TFrameBufferMemory *p,
				 u_int sx, u_int sy,
				 u_int lx, u_int ly,
				 u_int color)
{
	const u_int icol = tfbm_select_32_color(color);

	u_int y;
	for (y = sy; y < sy + ly; y++) {
		u_int x;
		for (x = sx; x < sx + lx; x++) {
			tfbm_set_pixel_32bpp_packed(p, x, y, icol);
		}
	}
}

void tfbm_clear_all_32bpp_packed(TFrameBufferMemory *p, const u_int color)
{
	tfbm_fill_rect_32bpp_packed(p, 0, 0, p->width, p->height, color);
}

static void tfbm_byte_to_8pix_32bpp_packed(TFrameBufferMemory *p,
					   const u_int x, const u_int y,
					   const u_int sb, const u_int icol)
{
	if (sb & 0x80)
		tfbm_set_pixel_32bpp_packed(p, x + 0, y, icol);

	if (sb & 0x40)
		tfbm_set_pixel_32bpp_packed(p, x + 1, y, icol);

	if (sb & 0x20)
		tfbm_set_pixel_32bpp_packed(p, x + 2, y, icol);

	if (sb & 0x10)
		tfbm_set_pixel_32bpp_packed(p, x + 3, y, icol);

	if (sb & 0x08)
		tfbm_set_pixel_32bpp_packed(p, x + 4, y, icol);

	if (sb & 0x04)
		tfbm_set_pixel_32bpp_packed(p, x + 5, y, icol);

	if (sb & 0x02)
		tfbm_set_pixel_32bpp_packed(p, x + 6, y, icol);

	if (sb & 0x01)
		tfbm_set_pixel_32bpp_packed(p, x + 7, y, icol);
}

static void tfbm_byte_to_xpix_32bpp_packed(TFrameBufferMemory *p,
					   const u_int x, const u_int y,
					   const u_int sb, const u_int icol,
					   const u_int index)
{
	switch (index) {
	case 7:
		if(sb & 0x02)
			tfbm_set_pixel_32bpp_packed(p, x + 6, y, icol);

	case 6:
		if(sb & 0x04)
			tfbm_set_pixel_32bpp_packed(p, x + 5, y, icol);

	case 5:
		if(sb & 0x08)
			tfbm_set_pixel_32bpp_packed(p, x + 4, y, icol);

	case 4:
		if(sb & 0x10)
			tfbm_set_pixel_32bpp_packed(p, x + 3, y, icol);

	case 3:
		if(sb & 0x20)
			tfbm_set_pixel_32bpp_packed(p, x + 2, y, icol);

	case 2:
		if(sb & 0x40)
			tfbm_set_pixel_32bpp_packed(p, x + 1, y, icol);

	case 1:
		if(sb & 0x80)
			tfbm_set_pixel_32bpp_packed(p, x + 0, y, icol);
	}
}

void tfbm_overlay_32bpp_packed(TFrameBufferMemory *p,
			       u_int xd, u_int yd,
			       const u_char *ps,
			       u_int lx, u_int ly,
			       u_int gap, u_int color)
{
	const u_int icol = tfbm_select_32_color(color);

	u_int x;
	u_int y;
	u_int i;
	for (y = yd; y < yd + ly; y++) {
		const u_char *tps = ps;
		x = xd;
		for (i = lx; i >= 8; i -= 8) {
			tfbm_byte_to_8pix_32bpp_packed(p, x, y, *tps, icol);

			tps++;
			x += 8;
		}

		if (i)
			tfbm_byte_to_xpix_32bpp_packed(p, x, y, *tps, icol, i);

		ps += gap;
	}
}

void tfbm_reverse_32bpp_packed(TFrameBufferMemory *p,
			       u_int sx, u_int sy,
			       u_int lx, u_int ly,
			       u_int color)
{
	const u_int icol = tfbm_select_32_color(color);

	u_int x,y;
	for (y = sy; y < sy + ly; y++) {
		for (x = sx; x < sx + lx ;x++) {
			tfbm_xor_pixel_32bpp_packed(p, x, y, icol);
		}
	}
}

#endif /* JFB_32BPP */

