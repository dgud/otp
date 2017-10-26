/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2017. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * %CopyrightEnd%
 */
#include <stdio.h>
#include <stdlib.h>
#include "wxe_driver.h"

void push_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[], int op, void (*fptr) (ErlNifEnv *, ErlNifPid *, ERL_NIF_TERM *), int cast);

int get_ptr(ErlNifEnv* env, ERL_NIF_TERM term, void** dp)
{
    ErlNifUInt64 temp;
    if(enif_get_uint64(env, term, &temp)) {
        *dp = (void *) temp;
        return 1;
    } else return 0;
}

static ERL_NIF_TERM wx_setup_cmd(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int op, fi_a, cast;
    const ERL_NIF_TERM *fi_t;
    void * fptr;
    if(!(enif_get_tuple(env, argv[argc-2], &fi_a, &fi_t) && fi_a == 2))
        return enif_make_badarg(env);
    if(!enif_get_int(env, fi_t[0], &op) || !get_ptr(env, fi_t[1], &fptr))
        return enif_make_badarg(env);
    if(!enif_get_int(env, argv[argc-1], &cast))
        return enif_make_badarg(env);
    push_nif(env, argc-2, argv, op, fptr, cast);
    return enif_make_int(env, op);
}

static ErlNifFunc nif_funcs[] =
{
    {"queue_cmd", 1, wx_setup_cmd},
    {"queue_cmd", 2, wx_setup_cmd},
    {"queue_cmd", 3, wx_setup_cmd},
    {"queue_cmd", 4, wx_setup_cmd},
    {"queue_cmd", 5, wx_setup_cmd},
    {"queue_cmd", 6, wx_setup_cmd},
    {"queue_cmd", 7, wx_setup_cmd},
    {"queue_cmd", 8, wx_setup_cmd},
    {"queue_cmd", 9, wx_setup_cmd},
    {"queue_cmd",10, wx_setup_cmd},
    {"queue_cmd",11, wx_setup_cmd},
    {"queue_cmd",12, wx_setup_cmd},
    {"queue_cmd",13, wx_setup_cmd},
    {"queue_cmd",14, wx_setup_cmd}
};

ERL_NIF_INIT(wxe_util,nif_funcs,NULL,NULL,NULL,NULL)
