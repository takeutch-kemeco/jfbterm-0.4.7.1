/*
 * JFBTERM - skipagent.h
 * Copyright (C) 2012-2014 Takeutch Kemeco
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

#ifndef __SKIPAGENT_H__
#define __SKIPAGENT_H__

typedef int (*sage_throw_func)(void*);

struct SkipAgentContext {
        int use_flag;
        int close_flag;
        int mutex_flag;
        int order_flag;
        unsigned long long start_time;
        unsigned long long cur_time;
        unsigned long long prev_time;
        sage_throw_func func;
        void* func_param;
};

extern struct SkipAgentContext skip_agent_context;

void init_skip_agent(struct SkipAgentContext* context);
void throw_skip_agent(struct SkipAgentContext* context, sage_throw_func func, void* param);
void close_skip_agent(struct SkipAgentContext* context);

#endif // __SKIPAGENT_H__

