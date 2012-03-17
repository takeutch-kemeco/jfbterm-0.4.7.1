/* indextbl.h 2012 Takeutch Kemeco
 * 3-clause BSD lisence
 */

#ifndef __INDEXTBL_H__
#define __INDEXTBL_H__

extern u_int** itbl_new(const u_int w, const u_int h,
                        const u_int bytePerPixel,
                        const u_int bytePerLine);
extern void itbl_free(u_int** a, const u_int h);

#endif /* __INDEXTBL_H__ */
