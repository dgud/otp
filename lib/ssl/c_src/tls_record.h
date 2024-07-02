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

#ifndef __TLS_RECORD_H__
#define __TLS_RECORD_H__

#include <erl_nif.h>

#include <stdbool.h>
#include <stdlib.h>

#ifdef DEBUG
#    include <assert.h>
#    define ASSERT(X) assert(X)
#else
#    define ASSERT(X)                                                          \
        do {                                                                   \
        } while (0)
#endif

#ifndef MAX
#    define MAX(A, B) ((A) > (B) ? (A) : (B))
#endif

#ifndef MIN
#    define MIN(A, B) ((A) < (B) ? (A) : (B))
#endif

typedef unsigned char byte;

#define TLS_RECORD_MAXIMUM_LENGTH (1 << 14)

enum tls_record_type {
    TLS_RECORD_TYPE_CHANGECIPHERSPEC = 20,
    TLS_RECORD_TYPE_ALERT = 21,
    TLS_RECORD_TYPE_HANDSHAKE = 22,

    /* !! TLS 1.3 has inner records under this banner, including handshake,
     * key update, et cetera. */
    TLS_RECORD_TYPE_APPLICATION = 23,

    /* Not supported, but we need to return it nevertheless to send a proper
     * alert. */
    TLS_RECORD_TYPE_HEARTBEAT = 24,

    TLS_RECORD_TYPE_FIRST = TLS_RECORD_TYPE_CHANGECIPHERSPEC,
    TLS_RECORD_TYPE_LAST = TLS_RECORD_TYPE_HEARTBEAT
};

typedef struct tls_record_base TLSRecordBase;

struct tls_record_base {
    void (*destroy)(TLSRecordBase *, ErlNifEnv *);
    ERL_NIF_TERM (*process)(TLSRecordBase *, ErlNifEnv *);
    ERL_NIF_TERM (*update)(TLSRecordBase *, ErlNifEnv *, ERL_NIF_TERM);

    ErlNifEnv *env;
    ERL_NIF_TERM crypto;
    ERL_NIF_TERM input;
    ERL_NIF_TERM output;
};

ERL_NIF_TERM create_pack(ErlNifEnv *env,
                         ErlNifResourceType *resource_type,
                         ERL_NIF_TERM crypto,
                         ERL_NIF_TERM input,
                         ERL_NIF_TERM output);
ERL_NIF_TERM create_unpack(ErlNifEnv *env,
                           ErlNifResourceType *resource_type,
                           ERL_NIF_TERM crypto,
                           ERL_NIF_TERM input,
                           ERL_NIF_TERM output);

extern ERL_NIF_TERM am_badarg;
extern ERL_NIF_TERM am_continue;
extern ERL_NIF_TERM am_finished;
extern ERL_NIF_TERM am_wait;

typedef struct {
    SysIOVec *vec;
    int vec_len;
    int vec_offset;
    size_t block_offset;
} SysIOVecPosition;

size_t tls_record_copy_vec(SysIOVecPosition *pos, size_t size, byte *data);

#endif /* __TLS_RECORD_H__ */
