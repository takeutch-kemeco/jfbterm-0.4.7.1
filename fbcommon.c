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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stddef.h>
#include <stdbool.h>
#include <fcntl.h>
#include <errno.h>
#include <math.h>
#include <signal.h>
#include <sys/user.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <linux/vt.h>
#include <linux/fb.h>

#include "fbcommon.h"
#include "message.h"
#include "util.h"

#include "fbdpsp.h"

enum TFBM_SCR_ROT_FLAG tfbm_scr_rot_flag;

static int tfbm_select_visual(TFrameBufferMemory *p,
			      struct fb_var_screeninfo *fbvs,
			      struct fb_fix_screeninfo *fbfs);

static TFrameBufferCapability sFBCapabilityList[] = {
#if (defined(JFB_32BPP) && defined(JFB_PACKED) && defined(JFB_TRUECOLOR))
	{
		32, FB_TYPE_PACKED_PIXELS, FB_VISUAL_TRUECOLOR,
		tfbm_fill_rect_32bpp_packed,
		tfbm_overlay_32bpp_packed,
		tfbm_clear_all_32bpp_packed,
		tfbm_reverse_32bpp_packed
	},
#endif
#if (defined(JFB_32BPP) && defined(JFB_PACKED) && defined(JFB_DIRECTCOLOR))
	{
		32, FB_TYPE_PACKED_PIXELS, FB_VISUAL_DIRECTCOLOR,
		tfbm_fill_rect_32bpp_packed,
		tfbm_overlay_32bpp_packed,
		tfbm_clear_all_32bpp_packed,
		tfbm_reverse_32bpp_packed
	},
#endif
	{
		0, FB_TYPE_PACKED_PIXELS, FB_VISUAL_PSEUDOCOLOR,
		NULL,
		NULL,
		NULL,
		NULL
	 }
};

#ifndef major	/* defined in sys/sysmacros.h - ukai 1999/10/27 */
#define major(dev) (((dev) >> 8) & 0xff)
#endif

static char* fbdn = NULL;

static unsigned short red[256], green[256], blue[256];
static struct fb_cmap ncmap = {0, 256, red, green, blue, NULL};

static bool cmapSaved = false;
static unsigned short ored[256], ogreen[256], oblue[256], otrans[256];
static struct fb_cmap ocmap = {0, 256, ored, ogreen, oblue, otrans};

static int tvisual = 0;

float fbgamma = 1.7;

static struct fb_var_screeninfo ovar;
static bool modified_var_screen_info = false;

/*---------------------------------------------------------------------------*/
static u_int trueColor32Table[16];
static u_short trueColor16Table[16];

#define CL_B0 0x80FF
#define CL_B1 0xFFFF
#define CL_F0 0x80FF
#define CL_F1 0xFFFF

static u_short red16[16]   = {0x0000, CL_B0, CL_B0, CL_B0,   CL_B1, CL_B1, CL_B1, CL_B1,   CL_F0, CL_F0, CL_F0, CL_F0,   CL_F1, CL_F1, CL_F1, CL_F1, };
static u_short green16[16] = {0x0000, CL_B0, CL_B1, CL_B1,   CL_B0, CL_B0, CL_B1, CL_B1,   CL_F0, CL_F0, CL_F1, CL_F1,   CL_F0, CL_F0, CL_F1, CL_F1, };
static u_short blue16[16]  = {0x0000, CL_B1, CL_B0, CL_B1,   CL_B0, CL_B1, CL_B0, CL_B1,   CL_F0, CL_F1, CL_F0, CL_F1,   CL_F0, CL_F1, CL_F0, CL_F1, };

static void tfbm_setup_color_table(struct fb_var_screeninfo *var)
{
	int i;
	for (i = 0; i < 16; i++) {
		const u_int r = ((u_int)red16[i]   >> (16 - var->red.length))   << var->red.offset;
		const u_int g = ((u_int)green16[i] >> (16 - var->green.length)) << var->green.offset;
		const u_int b = ((u_int)blue16[i]  >> (16 - var->blue.length))  << var->blue.offset;
		const u_int rgb = r | g | b;
		trueColor32Table[i] = rgb;
		trueColor16Table[i] = (u_short)rgb;
		print_message("color %d : %x, %x\n", i, trueColor32Table[i], trueColor16Table[i]);
	}
}

/*---------------------------------------------------------------------------*/
static void tfbm_get_var_screen_info(int fh, struct fb_var_screeninfo *var)
{
	if (ioctl(fh, FBIOGET_VSCREENINFO, var))
		print_strerror_and_exit("ioctl FBIOGET_VSCREENINFO");
}

static void tfbm_set_var_screen_info(int fh, struct fb_var_screeninfo *var)
{
	if (ioctl(fh, FBIOPUT_VSCREENINFO, var))
		print_strerror_and_exit("ioctl FBIOPUT_VSCREENINFO");
}

static void tfbm_get_fix_screen_info(int fh, struct fb_fix_screeninfo *fix)
{
	if (ioctl(fh, FBIOGET_FSCREENINFO, fix))
		print_strerror_and_exit("ioctl FBIOGET_FSCREENINFO");
}

static void tfbm_get_cmap(int fh, struct fb_cmap *cmap)
{
	if (ioctl(fh, FBIOGETCMAP, cmap))
		print_strerror_and_exit("ioctl FBIOGETCMAP");
}

static void tfbm_put_cmap(int fh, struct fb_cmap *cmap)
{
	if (ioctl(fh, FBIOPUTCMAP, cmap))
		print_strerror_and_exit("ioctl FBIOPUTCMAP");
}

static void tfbm_pan_display(int fh, struct fb_var_screeninfo *var)
{
	if (ioctl(fh, FBIOPAN_DISPLAY, var))
		print_strerror_and_exit("ioctl FBIOPAN_DISPLAY");
}

static void linear_palette(int bit)
{
	int i;
	int size = 256 >> (8 - bit);

	for (i = 0 ; i < size ; i++) {
		red[i] = green[i] = blue[i] = (u_short)(65535.0 * pow(i / (size - 1.0), fbgamma));
	}
}

extern void set_most_left(__u32 bpp, struct fb_bitfield a_pixel_field);

static void tfbm_initcolors(TFrameBufferMemory *p,
			    struct fb_var_screeninfo *fbvs,
			    struct fb_fix_screeninfo *fbfs)
{
	/* get colormap */
	if ((fbfs->visual == FB_VISUAL_DIRECTCOLOR) || (fbfs->visual == FB_VISUAL_PSEUDOCOLOR))
		tfbm_get_cmap(p->fh, &ncmap);

	if (fbvs->bits_per_pixel != 32)
		die("Oops: %i bit/pixel ???\n", fbvs->bits_per_pixel);

	if (fbfs->visual == FB_VISUAL_DIRECTCOLOR)
		linear_palette(8);

	/* set colormap */
	if ((fbfs->visual == FB_VISUAL_DIRECTCOLOR) || (fbfs->visual == FB_VISUAL_PSEUDOCOLOR))
		tfbm_put_cmap(p->fh, &ncmap);
}

static void tfbm_show_screeninfo(TFrameBufferMemory *p,
				 struct fb_var_screeninfo *fbvs,
				 struct fb_fix_screeninfo *fbfs)
{
#ifdef DEBUG
	u_int c;
	const char *t;
	const char *v;

	c = fbfs->type;
	t = c == FB_TYPE_PACKED_PIXELS ?	"Packed Pixels" :
	    c == FB_TYPE_PLANES ?		"Non interleaved planes" :
	    c == FB_TYPE_INTERLEAVED_PLANES ?	"Interleaved planes" :
	    c == FB_TYPE_TEXT ?			"Text/attributes" :
						"Unknown planes";
	c = fbfs->visual;
	v = c == FB_VISUAL_MONO01 ?		"Monochr. 1=Black 0=White" :
	    c == FB_VISUAL_MONO10 ?		"Monochr. 1=White 0=Black" :
	    c == FB_VISUAL_TRUECOLOR ?		"True color" :
	    c == FB_VISUAL_PSEUDOCOLOR ?	"Pseudo color (like atari)" :
	    c == FB_VISUAL_DIRECTCOLOR ?	"Direct colo" :
	    c == FB_VISUAL_STATIC_PSEUDOCOLOR ?	"Pseudo color readonly" :
						"Unknown Visual";
	print_message("===== Frame Buffer ===============================\n");
	print_message("NAME   : [%s]\n", fbfs->id);
	print_message("TYPE   : %s\n", t);
	print_message("VISUAL : %s\n", v);
	print_message("SMEM   : %p L=%u\n", fbfs->smem_start, fbfs->smem_len);
	print_message("MMIO   : %p L=%u\n", fbfs->mmio_start, fbfs->mmio_len);
	print_message("LLEN   : %u\n", fbfs->line_length);
	print_message("RESO   : %ux%u+%u+%u / %ux%u @ %u\n",
			fbvs->xres, fbvs->yres, fbvs->xoffset, fbvs->yoffset,
			fbvs->xres_virtual, fbvs->yres_virtual,
			fbvs->bits_per_pixel);
	print_message("RED    : %u @ %u %c\n",
			fbvs->red.offset, fbvs->red.length,
			fbvs->red.msb_right ? '-' : '+');
	print_message("GREEN  : %u @ %u %c\n",
			fbvs->green.offset, fbvs->green.length,
			fbvs->green.msb_right ? '-' : '+');
	print_message("BLUE   : %u @ %u %c\n",
			fbvs->blue.offset, fbvs->blue.length,
			fbvs->blue.msb_right ? '-' : '+');
	print_message("==================================================\n");
#endif /* DEBUG */
}

/*---------------------------------------------------------------------------*/
void tfbm_init(TFrameBufferMemory *p)
{
	char *env_fbdn;
	p->fh = -1;

	if((fbdn = (char*)malloc(16)) == NULL)
		die("malloc: %s\n", strerror(errno));

	memset(fbdn, 0, 16);

	if (NULL != (env_fbdn = getenv("FRAMEBUFFER"))) {
		strncpy(fbdn, env_fbdn, 15);
	} else {
		int fd;
		struct fb_con2fbmap c2m;
		struct vt_stat vstat;

		if (-1 == (p->ttyfd = util_privilege_open(&vuid, "/dev/tty0", O_RDWR))) {
			UTIL_FREE(fbdn);
			die("open /dev/tty0: %s\n", strerror(errno));
		}

		if (-1 == ioctl(p->ttyfd, VT_GETSTATE, &vstat)) {
			UTIL_FREE(fbdn);
			die("ioctl VT_GETSTATE");
		}

		c2m.console = vstat.v_active;

		if (-1 == (fd = util_privilege_open(&vuid, "/dev/fb0", O_RDWR))) {
			UTIL_FREE(fbdn);
			die("open /dev/fb0: %s\n", strerror(errno));
		}

		if (-1 == ioctl(fd, FBIOGET_CON2FBMAP, &c2m)) {
			perror("ioctl FBIOGET_CON2FBMAP");
			c2m.framebuffer = 0;
		}

		close(fd);
		snprintf(fbdn, 15, "/dev/fb%d", c2m.framebuffer);
	}
}

void tfbm_open(TFrameBufferMemory *p)
{
	struct stat st;
	struct fb_var_screeninfo fb_var;
	struct fb_fix_screeninfo fb_fix;

	if ((p->fh = util_privilege_open(&vuid, fbdn, O_RDWR)) == -1)
		die("open %s: %s\n", fbdn, strerror(errno));

	if (-1 == fstat(p->fh,&st))
		die("fstat(%s): %s\n", fbdn, strerror(errno));

	if (!S_ISCHR(st.st_mode) || major(st.st_rdev) != 29 /* FB_MAJOR */)
		die("%s: not a frame buffer device\n", fbdn);

	free(fbdn);

	tfbm_get_var_screen_info(p->fh, &fb_var);
	if (fb_var.yres_virtual != fb_var.yres) {
		memcpy((void*)&ovar,(void*)&fb_var,sizeof(ovar));
		fb_var.yres_virtual = fb_var.yres;
		fb_var.yoffset = 0;
		fb_var.activate = FB_ACTIVATE_NOW;
		modified_var_screen_info = true;
		tfbm_set_var_screen_info(p->fh, &fb_var);
	}
	tfbm_get_fix_screen_info(p->fh, &fb_fix);

	if ((fb_fix.visual == FB_VISUAL_DIRECTCOLOR) || (fb_fix.visual == FB_VISUAL_PSEUDOCOLOR)) {
		tfbm_get_cmap(p->fh, &ocmap);
		cmapSaved = true;
	}

	switch(tfbm_scr_rot_flag) {
	case TFBM_SCR_ROT_FLAG_CW:
	case TFBM_SCR_ROT_FLAG_CCW:
		swap_int((int*)&fb_var.xres,         (int*)&fb_var.yres);
		swap_int((int*)&fb_var.xres_virtual, (int*)&fb_var.yres_virtual);
		swap_int((int*)&fb_var.xoffset,      (int*)&fb_var.yoffset);
		swap_int((int*)&fb_var.height,       (int*)&fb_var.width);
		break;
	}

	switch(tfbm_scr_rot_flag) {
	case TFBM_SCR_ROT_FLAG_CW:
		{
			u_int tmp = fb_var.left_margin;
			fb_var.left_margin  = fb_var.lower_margin;
			fb_var.lower_margin = fb_var.right_margin;
			fb_var.right_margin = fb_var.upper_margin;
			fb_var.upper_margin = tmp;
		}
		break;

	case TFBM_SCR_ROT_FLAG_CCW:
		{
			u_int tmp = fb_var.left_margin;
			fb_var.left_margin  = fb_var.upper_margin;
			fb_var.upper_margin = fb_var.right_margin;
			fb_var.right_margin = fb_var.lower_margin;
			fb_var.lower_margin = tmp;
		}
		break;
	}

#ifdef DEBUG
	tfbm_show_screeninfo(p, &fb_var, &fb_fix);
#endif /* DUBUG */

	tvisual = tfbm_select_visual(p, &fb_var, &fb_fix);
	if (tvisual < 0)
		die("Oops: Unknown frame buffer ???\n");

	tfbm_setup_color_table(&fb_var);

	tfbm_initcolors(p, &fb_var, &fb_fix);

	/* fix: scanline length is not necessarily the same as display width */
	p->width  = fb_var.xres;
	p->height = fb_var.yres;
	p->bytePerLine = fb_fix.line_length;

	p->soff = (u_int)(fb_fix.smem_start) & (~PAGE_MASK);
	p->slen = (fb_fix.smem_len + p->soff + ~PAGE_MASK) & PAGE_MASK;
	p->smem = (u_char*)mmap(NULL, p->slen, PROT_READ|PROT_WRITE,
				MAP_SHARED, p->fh, (off_t)0);
	if ((ptrdiff_t)p->smem == -1) {
		die("cannot mmap(smem)");
	}
	p->smem = (u_char*)p->smem + p->soff;

	p->moff = (u_int)(fb_fix.mmio_start) & (~PAGE_MASK);
	p->mlen = (fb_fix.mmio_len + p->moff + ~PAGE_MASK) & PAGE_MASK;
	p->mmio = (u_char*)mmap(NULL, p->mlen, PROT_READ|PROT_WRITE,
				MAP_SHARED, p->fh, p->slen);
	if ((ptrdiff_t)p->mmio == -1) {
#ifdef JFB_MMIO_CHECK
		die("cannot mmap(mmio)");
#else
		print_message("cannot mmap(mmio) : %s\n", strerror(errno));
#endif /* JFB_MMIO_CHECK */
	}
	p->mmio = (u_char*)p->mmio + p->moff;

#ifdef DEBUG
	print_message("mmap ; %d - %p\n", p->slen, p->smem);
	print_message("mmio ; %d - %p\n", p->mlen, p->mmio);
#endif /* DEBUG */

	/* move viewport to upper left corner */
	if (fb_var.xoffset != 0 || fb_var.yoffset != 0) {
		fb_var.xoffset = 0;
		fb_var.yoffset = 0;
		tfbm_pan_display(p->fh, &fb_var);
	}

	if (!sFBCapabilityList[tvisual].fill)
		die("No framebuffer supported.");

	p->cap = sFBCapabilityList[tvisual];
}

void tfbm_close(TFrameBufferMemory *p)
{
	if (p->fh == -1)
		return;

	if ((ptrdiff_t)p->smem != -1)
		munmap((caddr_t)((ptrdiff_t)p->smem & PAGE_MASK), p->slen);

	if ((ptrdiff_t)p->mmio != -1)
		munmap((caddr_t)((ptrdiff_t)p->mmio & PAGE_MASK), p->mlen);

	if (cmapSaved == true) {
		tfbm_put_cmap(p->fh, &ocmap);
		cmapSaved = false;
	}

	if (modified_var_screen_info == true) {
		ovar.activate = FB_ACTIVATE_NOW;
		tfbm_set_var_screen_info(p->fh, &ovar);
		modified_var_screen_info = false;
	}

	close(p->fh);
}

int tfbm_select_visual(TFrameBufferMemory *p,
		       struct fb_var_screeninfo *fbvs,
		       struct fb_fix_screeninfo *fbfs)
{
	int ret;
	int static_color = 0;

	if(fbfs->visual == FB_VISUAL_STATIC_PSEUDOCOLOR){
	    fbfs->visual = FB_VISUAL_PSEUDOCOLOR;
	    static_color = 1;
	}

	for(ret = 0 ; sFBCapabilityList[ret].bitsPerPixel != 0 ; ret++) {
		if ((sFBCapabilityList[ret].fbType == fbfs->type) &&
		    (sFBCapabilityList[ret].fbVisual == fbfs->visual) &&
		    (sFBCapabilityList[ret].bitsPerPixel==fbvs->bits_per_pixel)) {
			if (static_color)
			    fbfs->visual = FB_VISUAL_STATIC_PSEUDOCOLOR;

			return ret;
		}
	}

	if (static_color)
		fbfs->visual = FB_VISUAL_STATIC_PSEUDOCOLOR;

	return -1;
}

u_int tfbm_select_32_color(u_int c)
{
	return trueColor32Table[c];
}

