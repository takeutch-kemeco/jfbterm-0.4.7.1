/* Copyright (C) 1999  Noritoshi MASUICHI (nmasu@ma3.justnet.ne.jp)
 * Copyright (C) 2012  Takeutch Kemeco (takeutchkemeco@gmail.com)
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

#include <sys/types.h>
#include "fbcommon.h"

#ifndef __FBDPSP32_H__

void tfbm_fill_rect_32bpp_packed(TFrameBufferMemory* p,
				 const u_int sx, const u_int sy,
				 const u_int lx, const u_int ly,
				 const u_int color);
void tfbm_overlay_32bpp_packed(TFrameBufferMemory* p,
			       const u_int xd, const u_int yd,
			       const u_char* ps,
			       const u_int lx, const u_int ly,
			       const u_int gap,
			       const u_int color);
void tfbm_clear_all_32bpp_packed(struct Raw_TFrameBufferMemory* p,
			         const u_int color);
void tfbm_reverse_32bpp_packed(TFrameBufferMemory* p,
			       const u_int sx, const u_int sy,
			       const u_int lx, const u_int ly,
			       const u_int color);

#endif /* __FBDPSP32_H__ */
