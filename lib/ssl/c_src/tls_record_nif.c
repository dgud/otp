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

#define TLS_RECORD_NIF_VERSION 1

static ErlNifResourceType *resource_type;

ERL_NIF_TERM am_badarg;
ERL_NIF_TERM am_continue;
ERL_NIF_TERM am_finished;
ERL_NIF_TERM am_wait;

static void destroy(ErlNifEnv *env, void *handle_) {
    TLSRecordBase *handle = (TLSRecordBase *)handle_;
    handle->destroy(handle, env);
}

static ERL_NIF_TERM process_nif(ErlNifEnv *env,
                                int argc,
                                const ERL_NIF_TERM argv[]) {
    TLSRecordBase *handle;

    if (enif_get_resource(env, argv[0], resource_type, (void **)&handle)) {
        return handle->process(handle, env);
    }

    return enif_raise_exception(env, enif_make_atom(env, "badarg"));
}

static ERL_NIF_TERM update_nif(ErlNifEnv *env,
                               int argc,
                               const ERL_NIF_TERM argv[]) {
    TLSRecordBase *handle;

    if (enif_get_resource(env, argv[0], resource_type, (void **)&handle)) {
        return handle->update(handle, env, argv[1]);
    }

    return enif_raise_exception(env, enif_make_atom(env, "badarg"));
}

static ERL_NIF_TERM create_pack_nif(ErlNifEnv *env,
                                    int argc,
                                    const ERL_NIF_TERM argv[]) {
    return create_pack(env, resource_type, argv[0], argv[1], argv[2]);
}

static ERL_NIF_TERM create_unpack_nif(ErlNifEnv *env,
                                      int argc,
                                      const ERL_NIF_TERM argv[]) {
    return create_unpack(env, resource_type, argv[0], argv[1], argv[2]);
}

static ErlNifFunc nif_funcs[] = {
        {"create_pack_nif", 3, create_pack_nif, 0},
        {"create_unpack_nif", 3, create_unpack_nif, 0},

        {"process_nif", 1, process_nif, 0},
        {"update_nif", 2, update_nif, 0},
};

static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {

    ErlNifResourceTypeInit callbacks = {.dtor = destroy,
                                        .stop = NULL,
                                        .down = NULL,
                                        .members = 4,
                                        .dyncall = NULL};

    const ERL_NIF_TERM *options;
    int arity, version;

    if (!enif_get_tuple(env, load_info, &arity, &options) ||
        arity != 2) {
        return 1;
    }

    if (!enif_get_int(env, options[0], &version) ||
        version != TLS_RECORD_NIF_VERSION) {
        return 1;
    }

    resource_type = enif_init_resource_type(env,
                                            "tls_record",
                                            &callbacks,
                                            ERL_NIF_RT_CREATE,
                                            NULL);

    am_badarg = enif_make_atom(env, "badarg");
    am_continue = enif_make_atom(env, "continue");
    am_finished = enif_make_atom(env, "finished");
    am_wait = enif_make_atom(env, "wait");

    return 0;
}

static int upgrade(ErlNifEnv *env,
                   void **priv_data,
                   void **old_priv_data,
                   ERL_NIF_TERM load_info) {
    return 0;
}

static void unload(ErlNifEnv *env, void *priv_data) {
}

ERL_NIF_INIT(tls_record_nif, nif_funcs, load, NULL, upgrade, unload)
