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

#define EXECUTION_BUDGET (1 << 16)

typedef struct {
    TLSRecordBase base;

    enum {
        TLS_UP_STAGE_HEADER,
        TLS_UP_STAGE_PAYLOAD,
        TLS_UP_STAGE_UNPACKED,
        TLS_UP_STAGE_CORRUPT,
    } stage;

    size_t budget;
    size_t progress;
    size_t target;
    byte *data;

    enum tls_record_type type;

    byte header[5];
    byte major;
    byte minor;
    int length;

    ErlNifBinary payload;
} TLS_UP_State;

static void destroy(TLSRecordBase *state_, ErlNifEnv *env) {
    TLS_UP_State *state = (TLS_UP_State *)state_;

    switch (state->stage) {
    case TLS_UP_STAGE_PAYLOAD:
    case TLS_UP_STAGE_UNPACKED:
        enif_release_binary(&state->payload);
        break;
    default:
        break;
    }

    enif_free_env(state->base.env);
}

static void restart_unpacking(TLS_UP_State *state) {
    state->stage = TLS_UP_STAGE_HEADER;
    state->data = state->header;
    state->target = sizeof(state->header);
    state->progress = 0;
}

static size_t process_record_layer(ErlNifIOVec *vec, void *state_) {
    TLS_UP_State *state = (TLS_UP_State *)state_;
    SysIOVecPosition pos = {.vec = vec->iov, .vec_len = vec->iovcnt};
    size_t copied, needed, dequeued;

    dequeued = 0;

    do {
        if (state->progress == state->target) {
            state->progress = 0;

            switch (state->stage) {
            case TLS_UP_STAGE_HEADER: {
                byte major, minor, type;
                size_t length;

                ASSERT(state->target == sizeof(state->header) &&
                       state->data == state->header);

                type = state->data[0];
                major = state->data[1];
                minor = state->data[2];
                length = ((size_t)state->data[3] << 8) |
                         ((size_t)state->data[4] << 0);

                if (!((type >= TLS_RECORD_TYPE_FIRST &&
                       type <= TLS_RECORD_TYPE_LAST) &&
                      (major == 3 && minor <= 3) &&
                      (length > 0 && length <= TLS_RECORD_MAXIMUM_LENGTH) &&
                      enif_alloc_binary(length, &state->payload))) {
                    state->stage = TLS_UP_STAGE_CORRUPT;
                    return vec->size;
                }

                state->type = (enum tls_record_type)type;
                state->major = major;
                state->minor = minor;
                state->length = length;

                state->stage = TLS_UP_STAGE_PAYLOAD;
                state->data = state->payload.data;
                state->target = length;
                break;
            }
            case TLS_UP_STAGE_PAYLOAD:
                /* We've got a full record, break out of the loop and start
                 * processing it. */
                ASSERT(state->target == state->length &&
                       state->data == state->payload.data);
                state->stage = TLS_UP_STAGE_UNPACKED;
                goto done;
            case TLS_UP_STAGE_UNPACKED:
            case TLS_UP_STAGE_CORRUPT:
                /* Should never be called in these stages. */
                abort();
            }
        }

        ASSERT(state->progress < state->target);
        needed = state->target - state->progress;
        copied = tls_record_copy_vec(&pos,
                                     MIN(needed, state->budget - dequeued),
                                     &state->data[state->progress]);
        state->progress += copied;
        dequeued += copied;
    } while (copied == needed);

done:
    ASSERT(state->budget > dequeued);
    state->budget -= dequeued;

    return dequeued;
}

static ERL_NIF_TERM process(TLSRecordBase *state_, ErlNifEnv *env) {
    TLS_UP_State *state = (TLS_UP_State *)state_;
    int events;

    events = ERL_NIF_IO_STREAM_EVENT_NONE;
    state->budget = EXECUTION_BUDGET;

    switch (state->stage) {
    case TLS_UP_STAGE_CORRUPT:
        return enif_raise_exception(env, am_badarg);
    case TLS_UP_STAGE_HEADER:
    case TLS_UP_STAGE_PAYLOAD:
        (void)enif_ios_read(env,
                            state->base.input,
                            process_record_layer,
                            (void *)state,
                            &events);

        /* TODO: consume according to how much we've done. */
        if (enif_consume_timeslice(env, 1)) {
            return enif_make_tuple2(env,
                                    enif_make_int(env, events),
                                    am_continue);
        }

        if (state->stage != TLS_UP_STAGE_UNPACKED) {
            /* We need more data. The caller should call us once more and then
             * wait for the writer to send us a message telling us to resume
             * processing.
             *
             * This is necessary to avoid a race where more data is written
             * between reading and notifying the writer. */
            return enif_make_tuple2(env, enif_make_int(env, events), am_wait);
        }

        /* !! Fall through !! */
    case TLS_UP_STAGE_UNPACKED:
        if (0 && state->type == TLS_RECORD_TYPE_APPLICATION) {
            /* TODO: If we have the means to decrypt this, do so and write the
             * result to the output stream, pretending that we need to yield
             * before returning the next record. */
            return enif_make_tuple2(env,
                                    enif_make_int(env, events),
                                    am_continue);
        }

        restart_unpacking(state);

        /* {events(), 'finished', {type(), {major(), minor()}, payload()}} */
        return enif_make_tuple3(
                env,
                enif_make_int(env, events),
                am_finished,
                enif_make_tuple3(
                        env,
                        enif_make_int(env, state->type),
                        enif_make_tuple2(env,
                                         enif_make_int(env, state->major),
                                         enif_make_int(env, state->minor)),
                        enif_make_binary(env, &state->payload)));
    }

    /* GCC is smart enough to figure out that when enum values are handled in
     * this switch, but not smart enough to see that the function always
     * returns a value when that is the case.
     *
     * Suppress the error by aborting since it can't be reached anyway. */
    abort();
}

static ERL_NIF_TERM update(TLSRecordBase *state_,
                           ErlNifEnv *env,
                           ERL_NIF_TERM args) {
    TLS_UP_State *state = (TLS_UP_State *)state_;
    (void)state;
    return enif_raise_exception(env, am_badarg);
}

ERL_NIF_TERM create_unpack(ErlNifEnv *env,
                           ErlNifResourceType *resource_type,
                           ERL_NIF_TERM crypto,
                           ERL_NIF_TERM input,
                           ERL_NIF_TERM output) {
    TLS_UP_State *state =
            (TLS_UP_State *)enif_alloc_resource(resource_type,
                                                sizeof(TLS_UP_State));

    state->base.env = enif_alloc_env();
    state->base.crypto = enif_make_copy(state->base.env, crypto);
    state->base.input = enif_make_copy(state->base.env, input);
    state->base.output = enif_make_copy(state->base.env, output);
    state->base.destroy = destroy;
    state->base.process = process;
    state->base.update = update;

    restart_unpacking(state);

    return enif_make_resource(env, state);
}
