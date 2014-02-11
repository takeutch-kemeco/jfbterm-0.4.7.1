/*
 * skipagent.c
 * Copyright (C) 2012, 2014 Takeutch Kemeco
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
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <pthread.h>
#include <time.h>
#include <signal.h>
#include "message.h"
#include "skipagent.h"

bool sage_use = true;

static pthread_t thread;

static const uint64_t bound_time = (1000 * 1000) * 16;

#define LOOP_TIME ((1000 * 1000) * 4)
static const struct timespec loop_time_ts = {
	.tv_sec  = 0,
	.tv_nsec = LOOP_TIME,
};

static volatile uint64_t cur_time = 0;
static volatile uint64_t prev_time = 0;

static volatile bool run_flag = false;
static volatile bool order_flag = false;

static sage_throw_func cur_func = NULL;
static void* cur_param = NULL;

static int mutex_tmp = 0;

static uint64_t rdtsc(void)
{
	volatile uint64_t RAX;
	__asm__ volatile("rdtsc;":"=r"(RAX));
	return RAX;
}

static void lock_mutex(const int id)
{
        while (1) {
                __sync_val_compare_and_swap(&mutex_tmp, 0, id);
                if (mutex_tmp == id)
                        return;

                nanosleep(&loop_time_ts, NULL);
        }
}

static void unlock_mutex(const int id)
{
        while (1) {
                __sync_val_compare_and_swap(&mutex_tmp, id, 0);
                if (mutex_tmp == 0)
                        return;

                nanosleep(&loop_time_ts, NULL);
        }
}

static void peace_loop(void)
{
        while (run_flag && order_flag)
                nanosleep(&loop_time_ts, NULL);
}

static void transient_loop(void)
{
        const int transient_loop_id = 1;

        while (run_flag) {
                cur_time = rdtsc();

                if (order_flag) {
                        if ((cur_time - prev_time) > bound_time) {
                                lock_mutex(transient_loop_id);
                                cur_func(cur_param);
                                order_flag = false;
                                unlock_mutex(transient_loop_id);

                                peace_loop();

                                prev_time = cur_time;
                        }
                }

                nanosleep(&loop_time_ts, NULL);
        }
}
 
static void* main_loop(void* _a)
{
        if (sage_use)
                transient_loop();

	return NULL;
}

void sage_throw(sage_throw_func func, void* param)
{
        const int sage_throw_mutex_id = 2;

        if (sage_use) {
		lock_mutex(sage_throw_mutex_id);
                cur_func = func;
                cur_param = param;
                order_flag = true;
                unlock_mutex(sage_throw_mutex_id);
	} else {
                func(param);
	}
}

void sage_init(void)
{
	run_flag = true;

	if (pthread_create(&thread, NULL, main_loop, NULL) != 0)
		die("sage_init(): thread create err\n");
}

void sage_close(void)
{
	run_flag = false;
	pthread_join(thread, NULL);
}
