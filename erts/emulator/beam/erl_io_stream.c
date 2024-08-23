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
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "global.h"

#include "erl_io_stream.h"

#include "bif.h"
#include "erl_bits.h"
#include "erl_io_queue.h"

#define ERTS_IO_STREAM_CONST(X) ((erts_aint32_t)(X))

/* FIXME: Add ERTS_IO_STREAM_*_BLOCKED once we've got MPSC going, we will
 * likely want it to be a fixed-size (ring-)buffer to simulate busy
 * distribution ports. */
#define ERTS_IO_STREAM_READER_NOTIFY       ERTS_IO_STREAM_CONST(1 << 0)
#define ERTS_IO_STREAM_WRITER_NOTIFY       ERTS_IO_STREAM_CONST(1 << 1)
#define ERTS_IO_STREAM_READER_CLOSED       ERTS_IO_STREAM_CONST(1 << 2)
#define ERTS_IO_STREAM_WRITER_CLOSED       ERTS_IO_STREAM_CONST(1 << 3)
#define ERTS_IO_STREAM_READER_DEAD         ERTS_IO_STREAM_CONST(1 << 4)
#define ERTS_IO_STREAM_WRITER_DEAD         ERTS_IO_STREAM_CONST(1 << 5)

typedef struct erts_io_stream {
    erts_atomic32_t state;

    /* We use a standard IO queue wrapped with a lock for now, we may want a
     * more clever MPSC implementation later on, but this will suffice during
     * prototyping. */
    erts_mtx_t lock;
    ErtsIOQueue queue;
} ErtsIOStream;

struct erts_iostream_reader {
    ErtsIOStream *shared;
};

struct erts_iostream_writer {
    ErtsIOStream *shared;
};

static void ioq_shared_destructor(ErtsIOStream *shared) {
    erts_ioq_clear(&shared->queue);
    erts_free(ERTS_ALC_T_IOQ, shared);
}

static int ioq_shared_read_destructor(Binary *bin) {
    ErtsIOStreamReader *reader;
    ErtsIOStream *shared;

    reader = (ErtsIOStreamReader*)ERTS_MAGIC_BIN_DATA(bin);
    shared = reader->shared;

    if (erts_atomic32_read_bor_nob(&shared->state,
                                   ERTS_IO_STREAM_READER_CLOSED |
                                   ERTS_IO_STREAM_READER_DEAD)
        & ERTS_IO_STREAM_WRITER_DEAD) {
        /* Both ends are unreachable, destroy the queue. */
        ioq_shared_destructor(shared);
    }

    return 1;
}

static int ioq_shared_write_destructor(Binary *bin) {
    ErtsIOStreamWriter *writer;
    ErtsIOStream *shared;

    writer = (ErtsIOStreamWriter*)ERTS_MAGIC_BIN_DATA(bin);
    shared = writer->shared;

    if (erts_atomic32_read_bor_nob(&shared->state,
                                   ERTS_IO_STREAM_WRITER_CLOSED |
                                   ERTS_IO_STREAM_WRITER_DEAD)
        & ERTS_IO_STREAM_READER_DEAD) {
        /* Both ends are unreachable, destroy the queue. */
        ioq_shared_destructor(shared);
    }

    return 1;
}

void erts_io_stream_create(Binary **reader, Binary **writer) {
    ErtsIOStream *shared;
    ErtsIOStreamWriter *in;
    ErtsIOStreamReader *out;

    shared = erts_alloc(ERTS_ALC_T_IOQ, sizeof(ErtsIOStream));

    (*writer) = erts_create_magic_binary(sizeof(ErtsIOStreamWriter),
                                         ioq_shared_write_destructor);
    in = (ErtsIOStreamWriter*)ERTS_MAGIC_BIN_DATA(*writer);

    (*reader) = erts_create_magic_binary(sizeof(ErtsIOStreamReader),
                                         ioq_shared_read_destructor);
    out = (ErtsIOStreamReader*)ERTS_MAGIC_BIN_DATA(*reader);

    erts_atomic32_init_nob(&shared->state, 0);
    erts_mtx_init(&shared->lock,
                  "iostream_lock",
                  NIL,
                  ERTS_LOCK_FLAGS_CATEGORY_IO);
    erts_ioq_init(&shared->queue, ERTS_ALC_T_IOQ, 0);

    in->shared = shared;
    out->shared = shared;
}

ErtsIOStreamReader *erts_io_stream_get_reader(Eterm handle) {
    if (is_internal_magic_ref(handle)) {
        Binary *bin = erts_magic_ref2bin(handle);

        if (ERTS_MAGIC_BIN_DESTRUCTOR(bin) == ioq_shared_read_destructor) {
            return (ErtsIOStreamReader*)ERTS_MAGIC_BIN_DATA(bin);
        }
    }

    return NULL;
}

ErtsIOStreamWriter *erts_io_stream_get_writer(Eterm handle) {
    if (is_internal_magic_ref(handle)) {
        Binary *bin = erts_magic_ref2bin(handle);

        if (ERTS_MAGIC_BIN_DESTRUCTOR(bin) == ioq_shared_write_destructor) {
            return (ErtsIOStreamWriter*)ERTS_MAGIC_BIN_DATA(bin);
        }
    }

    return NULL;
}

void erts_io_stream_close_reader(ErtsIOStreamReader *reader) {
    /* `relb` to match the `acqb` in the read/write functions.
     *
     * This ensures that a notifications are properly ordered with regards to
     * those functions: a notification that is set between two calls will
     * neither be reordered before the first, nor after the second. */
    (void)erts_atomic32_read_bor_relb(&(reader->shared)->state,
                                      ERTS_IO_STREAM_READER_CLOSED);
}

void erts_io_stream_close_writer(ErtsIOStreamWriter *writer) {
    /* `relb`: see erts_io_stream_close_reader */
    (void)erts_atomic32_read_bor_relb(&(writer->shared)->state,
                                      ERTS_IO_STREAM_WRITER_CLOSED);
}

void erts_io_stream_notify_writer(ErtsIOStreamReader *reader) {
    /* `relb`: see erts_io_stream_close_reader */
    (void)erts_atomic32_read_bor_relb(&(reader->shared)->state,
                                      ERTS_IO_STREAM_WRITER_NOTIFY);
}

void erts_io_stream_notify_reader(ErtsIOStreamWriter *writer) {
    /* `relb`: see erts_io_stream_close_reader */
    (void)erts_atomic32_read_bor_relb(&(writer->shared)->state,
                                      ERTS_IO_STREAM_READER_NOTIFY);
}

int erts_io_stream_read(ErtsIOStreamReader *reader,
                        void (*callback)(ErtsIOQueue *queue, void *data),
                        void *data) {
    ErtsIOStream *shared = reader->shared;
    ErtsIOQueue *queue = &shared->queue;
    erts_aint32_t state;
    int events;

    erts_mtx_lock(&shared->lock);
    {
        /* Run the callback before checking if the writer is closed; we may
         * want to handle the data that was remaining in the queue and call
         * this function several times until we've finished it all. */
        callback(queue, data);
    }
    erts_mtx_unlock(&shared->lock);

    /* `acqb` to match the `relb` in the close and notification functions. See
     * erts_io_stream_close_reader for more details.
     *
     * Note that we check the state after reading, as there is a possibility
     * that the other end has already acted on the results of calling our
     * callback. */
    state = erts_atomic32_read_band_acqb(&shared->state,
                                         ~ERTS_IO_STREAM_READER_NOTIFY);
    events = ERTS_IO_STREAM_EVENT_NONE;

    if (state & ERTS_IO_STREAM_WRITER_CLOSED) {
        events |= ERTS_IO_STREAM_EVENT_CLOSED;
    }

    if (state & ERTS_IO_STREAM_READER_NOTIFY) {
        events |= ERTS_IO_STREAM_EVENT_NOTIFY;
    }

    return events;
}

int erts_io_stream_write(ErtsIOStreamWriter *writer,
                         ErtsIOVec *vec,
                         Uint skip) {
    ErtsIOStream *shared = writer->shared;
    ErtsIOQueue *queue = &shared->queue;
    erts_aint32_t state;
    int events;

    erts_mtx_lock(&shared->lock);
    {
        /* Enqueue data even if the reader is closed; this simplifies handling
         * notification races, and only results in one extra enqueue in the
         * worst case. */
        erts_ioq_enqv(queue, vec, skip);
    }
    erts_mtx_unlock(&shared->lock);

    /* `acqb` to match the `relb` in the close and notification functions. See
     * erts_io_stream_close_reader for more details.
     *
     * Note that we check the state after reading, as there is a possibility
     * that the other end has already consumed all the data that we enqueued
     * above and notified us that they need more data. */
    state = erts_atomic32_read_band_acqb(&shared->state,
                                         ~ERTS_IO_STREAM_WRITER_NOTIFY);
    events = ERTS_IO_STREAM_EVENT_NONE;

    if (state & ERTS_IO_STREAM_READER_CLOSED) {
        events |= ERTS_IO_STREAM_EVENT_CLOSED;
    }

    if (state & ERTS_IO_STREAM_WRITER_NOTIFY) {
        events |= ERTS_IO_STREAM_EVENT_NOTIFY;
    }

    return events;
}

BIF_RETTYPE iostream_new_0(BIF_ALIST_0) {
    Binary *reader_bin, *writer_bin;
    Eterm reader, writer;
    Eterm *hp;

    hp = HAlloc(BIF_P, 3 + (ERTS_MAGIC_REF_THING_SIZE * 2));
    erts_io_stream_create(&reader_bin, &writer_bin);

    reader = erts_mk_magic_ref(&hp, &MSO(BIF_P), reader_bin);
    writer = erts_mk_magic_ref(&hp, &MSO(BIF_P), writer_bin);

    BIF_RET(TUPLE2(hp, reader, writer));
}

BIF_RETTYPE iostream_close_1(BIF_ALIST_1) {
    ErtsIOStreamReader *reader;
    ErtsIOStreamWriter *writer;

    reader = erts_io_stream_get_reader(BIF_ARG_1);
    if (reader) {
        erts_io_stream_close_reader(reader);
        BIF_RET(am_ok);
    }

    writer = erts_io_stream_get_writer(BIF_ARG_1);
    if (writer) {
        erts_io_stream_close_writer(writer);
        BIF_RET(am_ok);
    }

    BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE iostream_notify_1(BIF_ALIST_1) {
    ErtsIOStreamReader *reader;
    ErtsIOStreamWriter *writer;

    reader = erts_io_stream_get_reader(BIF_ARG_1);
    if (reader) {
        erts_io_stream_notify_writer(reader);
        BIF_RET(am_ok);
    }

    writer = erts_io_stream_get_writer(BIF_ARG_1);
    if (writer) {
        erts_io_stream_notify_reader(writer);
        BIF_RET(am_ok);
    }

    BIF_ERROR(BIF_P, BADARG);
}

struct iostream_rti_args {
    Eterm result;
    UWord size;
    Process *p;
};

/* FIXME: this can easily be made yielding once we're lock-free, as the
 * underlying IO queue will only be modified by the reader: I'm thinking of
 * having something similar to the outer/middle signal queue setup. */
static void iostream_read_to_iovec(ErtsIOQueue *queue, void *data) {
    struct iostream_rti_args *args = (struct iostream_rti_args*)data;
    ErtsHeapFactory hfact;
    ErtsIOVec vec;

    UWord bytes_remaining, to_dequeue;
    Eterm *tail_ptr;

    (void)erts_ioq_peekqv(queue, &vec);

    if (args->size == ERTS_UWORD_MAX) {
        to_dequeue = vec.common.size;
    } else if (args->size <= vec.common.size) {
        to_dequeue = args->size;
    } else {
        Eterm *hp = HAlloc(args->p, 3);
        args->result = TUPLE2(hp,
                              am_incomplete,
                              erts_make_integer(vec.common.size, args->p));
        return;
    }

    bytes_remaining = to_dequeue;
    tail_ptr = &args->result;
    args->result = NIL;

    erts_factory_proc_prealloc_init(&hfact,
                                    args->p,
                                    (vec.common.vsize *
                                     (2 + ERL_REFC_BITS_SIZE)));

    for (int i = 0; i < vec.common.vsize && bytes_remaining > 0; i++) {
        SysIOVec *iov_entry = &queue->v_head[i];
        Eterm element;
        Uint size;
        Eterm *hp;

        ASSERT(IS_BINARY_SIZE_OK(iov_entry->iov_len));
        size = MIN(iov_entry->iov_len, bytes_remaining);
        bytes_remaining -= size;
        size = NBITS(size);

        if (iov_entry->iov_len > ERL_ONHEAP_BINARY_LIMIT) {
            ErtsIOQBinary *ref_bin = queue->b_head[i];

            erts_refc_inc(&ref_bin->nif.intern.refc, 2);

            hp = erts_produce_heap(&hfact, 2 + ERL_REFC_BITS_SIZE, 0);
            element = erts_wrap_refc_bitstring(&hfact.off_heap->first,
                                               &hfact.off_heap->overhead,
                                               &hp,
                                               &ref_bin->nif,
                                               (byte*)iov_entry->iov_base,
                                               0,
                                               size);
        } else {
            hp = erts_produce_heap(&hfact, 2 + heap_bits_size(size), 0);
            element = HEAP_BITSTRING(&hp[2], iov_entry->iov_base, 0, size);
        }

        *tail_ptr = CONS(hp, element, NIL);
        tail_ptr = &CDR(hp);
    }

    (void)erts_ioq_deq(queue, to_dequeue);
    erts_factory_close(&hfact);
}

BIF_RETTYPE iostream_read_internal_2(BIF_ALIST_1) {
    struct iostream_rti_args args = { .result = NIL, .p = BIF_P };
    ErtsIOStreamReader *reader;
    int events;
    Eterm *hp;

    reader = erts_io_stream_get_reader(BIF_ARG_1);
    if (reader == NULL) {
        BIF_ERROR(BIF_P, BADARG);
    }

    if (BIF_ARG_2 == am_all) {
        args.size = ERTS_UWORD_MAX;
    } else if (!term_to_UWord(BIF_ARG_2, &args.size) ||
               !IS_BINARY_SIZE_OK(args.size)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    events = erts_io_stream_read(reader,
                                 iostream_read_to_iovec,
                                 (void*)&args);

    hp = HAlloc(BIF_P, 3);
    BIF_RET(TUPLE2(hp, make_small(events), args.result));
}

#define SMALL_WRITE_VEC 16

/* FIXME: make this yielding and require iovecs later on. */
BIF_RETTYPE iostream_write_internal_2(BIF_ALIST_1) {
    ErtsIOStreamWriter *writer;
    Uint csize, pvsize, pcsize;
    size_t total_size;
    int vsize;

    writer = erts_io_stream_get_writer(BIF_ARG_1);
    if (writer == NULL) {
        BIF_ERROR(BIF_P, BADARG);
    }

    if (!erts_ioq_iolist_vec_len(BIF_ARG_2,
                                 &vsize,
                                 &csize,
                                 &pvsize,
                                 &pcsize,
                                 &total_size,
                                 ERL_ONHEAP_BINARY_LIMIT)) {
        int events;

        ErtsIOQBinary *bv[SMALL_WRITE_VEC];
        SysIOVec iv[SMALL_WRITE_VEC];
        ErtsIOQBinary *cbin;
        ErtsIOVec vec;

        if (pcsize > 0) {
            cbin = (ErtsIOQBinary*)erts_bin_nrml_alloc(pcsize);
        } else {
            cbin = NULL;
        }

        if (pvsize < SMALL_WRITE_VEC) {
            vec.common.iov = &iv[0];
            vec.common.binv = &bv[0];
        } else {
            size_t alloc_size, binv_offset;
            char *ptr;

            binv_offset = ERTS_ALC_DATA_ALIGN_SIZE((pvsize + 1) *
                                                    sizeof(SysIOVec));
            alloc_size = binv_offset;
            alloc_size += (pvsize + 1) * sizeof(ErtsIOQBinary*);

            ptr = erts_alloc(ERTS_ALC_T_TMP, alloc_size);

            vec.common.iov = (SysIOVec*)ptr;
            vec.common.binv = (ErtsIOQBinary**)&ptr[binv_offset];
        }

        vec.common.vsize = erts_ioq_iodata_to_vec(BIF_ARG_2,
                                                  vec.common.iov,
                                                  vec.common.binv,
                                                  cbin,
                                                  ERL_ONHEAP_BINARY_LIMIT,
                                                  0);
        ERTS_ASSERT(vec.common.vsize == pvsize);
        vec.common.size = total_size;

        events = erts_io_stream_write(writer, &vec, 0);

        ASSERT((vec.common.iov == &iv[0]) == (vec.common.binv == &bv[0]));
        if (vec.common.iov != &iv[0]) {
            erts_free(ERTS_ALC_T_TMP, vec.common.iov);
        }

        BIF_RET(make_small(events));
    }

    BIF_ERROR(BIF_P, BADARG);
}
