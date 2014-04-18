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

#ifndef INCLUDE_FBCOMMON_H
#define INCLUDE_FBCOMMON_H

#include <sys/types.h>

struct Raw_TFrameBufferMemory;

typedef struct Raw_TFrameBufferMemory {
	u_int height;
	u_int width;
	u_int bytePerLine;
	u_int screen_rotate;
	/* --- */
	int fh;
	u_int sstart;
	u_int soff;
	u_int slen;
	u_int mstart;
	u_int moff;
	u_int mlen;
	int ttyfd;
	u_char *smem;
	u_char *mmio;
} TFrameBufferMemory;

enum TFBM_SCR_ROT_FLAG {
	TFBM_SCR_ROT_FLAG_NORMAL = 0x01,
	TFBM_SCR_ROT_FLAG_CW     = 0x02,
	TFBM_SCR_ROT_FLAG_CCW    = 0x04,
};

extern TFrameBufferMemory gFramebuffer;

void tfbm_init(TFrameBufferMemory *p);
void tfbm_open(TFrameBufferMemory *p);
void tfbm_close(TFrameBufferMemory *p);

extern u_int trueColor32Table[16];

#endif /* INCLUDE_FBCOMMON_H */
