/* skipagent.h 2012 Takeutch Kemeco
 * 3-clause BSD lisence
 */

#include <stdbool.h>

#ifndef __SKIPAGENT_H__
#define __SKIPAGENT_H__

extern bool sage_use;

typedef void (*sage_throw_func)(void*);

void sage_init(void);
void sage_close(void);
void sage_throw(sage_throw_func func, void* param);

#endif // __SKIPAGENT_H__
