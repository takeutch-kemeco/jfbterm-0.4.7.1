/* indextbl.c 2012 Takeutch Kemeco
 * 3-clause BSD lisence
 */

#include <stdlib.h>
#include <sys/types.h>
#include "fbcommon.h"
#include "message.h"

/* xとyをスクリーンの回転設定に合わせて、右か左に回転移動。
 * 
 * CW（クロックワイズ）が、首を時計回りに傾けて見るのに適した状態。
 * CCW（カウンターCW）が、首を反時計回りに傾けて見るのに適した状態。
 */
static void rot_xy(u_int* real_x, u_int* real_y,
		   const u_int x, const u_int y,
	           const u_int w, const u_int h)
{
	switch(tfbm_scr_rot_flag) {
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

/* x, y 座標に対応した、p->smem[i] のための i を得る
 *
 * w, h は縦横ピクセル最大値
 * bytePerPixel は１ピクセルあたりのバイト数。（32bppなら4, 24bppなら 3, 16bppなら 2）
 * bytePerLine は１ラインあたりのバイト数。
 *
 * 備考：
 * bytePerLine はアライメントの関係で w * bytePerPixel とならない場合を考えて、
 * わざわざ引数で指定することとした。
 *
 * bytePerPixel で、15bpp や 2bpp などの場合にどう指定すべきかは
 * その辺りのソースコードを読んでないのでよくわからない。
 */
static u_int seek_pix_index(const u_int x, const u_int y,
                            const u_int w, const u_int h,
                            const u_int bytePerPixel,
                            const u_int bytePerLine)
{
	u_int real_x;
	u_int real_y;
	rot_xy(&real_x, &real_y, x, y, w, h);

	return (real_y * bytePerLine) + (real_x * bytePerPixel);
}

/* p->smem[i] の i に対応する x,y 変換表を作成し返す
 *
 * これは u_int の配列に x 座標の対応表が格納され、この配列の配列が y 座標に対応。
 * 
 * 例：
 * a = itbl_new(p);
 * *(a + 3);         // y 座標が 3 の場合の、x座標対応表の配列への先頭アドレス
 * *(*(a + 3) + 2);  // y = 3, x = 2 の場合の、p->smem[i] の iに対応するインデックス
 *
 * 失敗した場合はエラーを表示してシステム全体を終了
 */
u_int** itbl_new(const u_int w, const u_int h,
                 const u_int bytePerPixel,
                 const u_int bytePerLine)
{
        u_int** a = malloc(sizeof(*a) * h);
        if(a == NULL) {
                die("error itbl_new(): a = malloc \n");
        }

	int j;
	for(j = 0; j < h; j++) {
                *(a + j) = malloc(sizeof(**a) * w);
                if(*(a + j) == NULL) {
                        die("error itbl_new(): p = malloc \n");
                }

                u_int* p = *(a + j);
		int i;
		for(i = 0; i < w; i++) {
			*(p + i) = seek_pix_index(i, j, w, h,
                                                  bytePerPixel,
                                                  bytePerLine);
		}
	}

        return a;
}

/* テーブルを開放 */
void itbl_free(u_int** a, const u_int h)
{
        if(a == NULL) {
                return;
        }

	int j;
	for(j = 0; j < h; j++) {
		free(*(a + j));
	}

	free(a);
}
