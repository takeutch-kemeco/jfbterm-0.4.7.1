/* skipagent.c 2012 Takeutch Kemeco
 * 3-clause BSD lisence
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <pthread.h>
#include <time.h>
#include <signal.h>
#include "message.h"
#include "skipagent.h"

bool sage_use = true;

static pthread_t thread;

static const unsigned long long loop_time  = (1000 * 1000) * 4;
static const unsigned long long limit_time = (1000 * 1000) * 200;
static const unsigned long long bound_time = (1000 * 1000) * 16;

static unsigned long long start_time = 0;
static unsigned long long cur_time   = 0;
static unsigned long long prev_time  = 0;

static bool run_flag = false;
static bool order_flag = false;

static sage_throw_func cur_func;
static void* cur_param = NULL;

static unsigned long long rdtsc(void)
{
	unsigned long long A;
	__asm__ volatile("rdtsc;":"=r"(A));
	return A;
}

static bool is_skip(void)
{
	if(cur_time - start_time < limit_time) {
		if(cur_time - prev_time < bound_time) {
			return true;
		} else {
			return false;
		}
	}

	return false;
}

static void* main_loop(void* _a)
{
	const struct timespec loop_time_ts = {
		.tv_sec  = 0,
		.tv_nsec = loop_time,
	};

	while(run_flag) {
		cur_time = rdtsc();

		if(order_flag) {
			if(is_skip() == false) {
				cur_func(cur_param);
				nanosleep(&loop_time_ts, NULL);
				cur_func(cur_param);

				prev_time = rdtsc();
				order_flag = false;
			}

			prev_time = cur_time;
		}

		nanosleep(&loop_time_ts, NULL);
	}

	return NULL;
}

void sage_throw(sage_throw_func func, void* param)
{
	switch(sage_use) {
	case true:
		cur_func = func;
		cur_param = param;

		start_time = rdtsc();
		order_flag = true;

		break;

	case false:
		func(param);
		break;
	}
}

void sage_init(void)
{
	start_time = 0;
	cur_time = 0;
	prev_time = 0;

	order_flag = false;
	run_flag = true;

	cur_param = NULL;

	pthread_attr_t attr;
	pthread_attr_init(&attr);
	pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);

	if(pthread_create(&thread, &attr, main_loop, NULL) != 0) {
		die("sage_init(): thread create err\n");
	}
}

void sage_close(void)
{
	run_flag = false;
}
