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

static const unsigned long long limit_time = (1000 * 1000) * 100;
static const unsigned long long bound_time = (1000 * 1000) * 16;

#define LOOP_TIME ((1000 * 1000) * 4)
static const struct timespec loop_time_ts = {
	.tv_sec  = 0,
	.tv_nsec = LOOP_TIME,
};

#define REFRESH_TIME ((1000 * 1000) * 16)
static const struct timespec refresh_time_ts = {
	.tv_sec  = 0,
	.tv_nsec = REFRESH_TIME,
};

static volatile unsigned long long start_time = 0;
static volatile unsigned long long cur_time   = 0;
static volatile unsigned long long prev_time  = 0;

static volatile bool run_flag = false;
static volatile bool order_flag = false;
static volatile bool refresh_flag = false;

static sage_throw_func cur_func = NULL;
static void* cur_param = NULL;

static unsigned long long rdtsc(void)
{
	volatile unsigned long long A;
	__asm__ volatile("rdtsc;":"=r"(A));
	return A;
}

static bool is_skip(void)
{
	if(cur_time - prev_time < bound_time) {
		if(cur_time - start_time >= limit_time) {
			return false;
		} else {
			return true;
		}
	}

	return false;
}

static void* main_loop(void* _a)
{
	while(run_flag) {
		if(order_flag) {
			cur_time = rdtsc();
			if(is_skip() == false) {
				order_flag = false;
				refresh_flag = true;
			}

			prev_time = cur_time;
		}

		if(refresh_flag) {
			refresh_flag = false;

			nanosleep(&refresh_time_ts, NULL);
			cur_func(cur_param);
		}

		nanosleep(&loop_time_ts, NULL);
	}

	run_flag = true;

	return NULL;
}

void sage_throw(sage_throw_func func, void* param)
{
	switch(sage_use) {
	case true:
		if(run_flag) {
			order_flag = true;

			if(order_flag == true) {
				cur_param = param;
				cur_func = func;
			}
		}
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

	cur_func = NULL;
	cur_param = NULL;

	if(pthread_create(&thread, NULL, main_loop, NULL) != 0) {
		die("sage_init(): thread create err\n");
	}
}

static void dummy_func(void* _a) {}

void sage_close(void)
{
	run_flag = false;
	cur_func = dummy_func;

	pthread_join(thread, NULL);
}
