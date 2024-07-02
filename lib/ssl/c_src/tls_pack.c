/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2024. All Rights Reserved.
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
 *
 */

#include "tls_record.h"

typedef struct {
    TLSRecordBase base;
} TLSRecordPack;

static void destroy(TLSRecordBase *state_, ErlNifEnv *env) {
    TLSRecordPack *state = (TLSRecordPack *)state_;
    enif_free_env(state->base.env);
}

static ERL_NIF_TERM process(TLSRecordBase *state_, ErlNifEnv *env) {
    TLSRecordPack *state = (TLSRecordPack *)state_;
    (void)state;
    return enif_raise_exception(env, am_badarg);
}

static ERL_NIF_TERM update(TLSRecordBase *state_,
                           ErlNifEnv *env,
                           ERL_NIF_TERM args) {
    TLSRecordPack *state = (TLSRecordPack *)state_;
    (void)state;
    return enif_raise_exception(env, am_badarg);
}

ERL_NIF_TERM create_pack(ErlNifEnv *env,
                         ErlNifResourceType *resource_type,
                         ERL_NIF_TERM crypto,
                         ERL_NIF_TERM input,
                         ERL_NIF_TERM output) {
    TLSRecordPack *state =
            (TLSRecordPack *)enif_alloc_resource(resource_type,
                                                 sizeof(TLSRecordPack));

    state->base.env = enif_alloc_env();
    state->base.crypto = enif_make_copy(state->base.env, crypto);
    state->base.input = enif_make_copy(state->base.env, input);
    state->base.output = enif_make_copy(state->base.env, output);
    state->base.destroy = destroy;
    state->base.process = process;
    state->base.update = update;

    return enif_make_resource(env, state);
}
