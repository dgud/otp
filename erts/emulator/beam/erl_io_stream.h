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

/*
 * Description: A stream used for transferring binary data between processes
 * without having to pass individual messages for each unit of data, nor being
 * forced to bring it onto process heaps for processing.
 */

#ifndef __ERL_IO_STREAM_H__
#define __ERL_IO_STREAM_H__

#include "sys.h"
#include "erl_term.h"

#define ERTS_IO_QUEUE_TYPES_ONLY__
#include "erl_io_queue.h"
#undef ERTS_IO_QUEUE_TYPES_ONLY__

typedef struct erts_iostream_reader ErtsIOStreamReader;
typedef struct erts_iostream_writer ErtsIOStreamWriter;

#define ERTS_IO_STREAM_EVENT_NONE        (0)
#define ERTS_IO_STREAM_EVENT_CLOSED      (1 << 0)
#define ERTS_IO_STREAM_EVENT_NOTIFY      (1 << 1)

void erts_io_stream_create(Binary **reader,
                           Binary **writer);

ErtsIOStreamReader *erts_io_stream_get_reader(Eterm handle);
ErtsIOStreamWriter *erts_io_stream_get_writer(Eterm handle);

/** @brief Closes the respective end of the stream, notifying the other end
 * that processing needs to stop. */
void erts_io_stream_close_reader(ErtsIOStreamReader *reader);
/** @copydoc erts_io_stream_close_reader */
void erts_io_stream_close_writer(ErtsIOStreamWriter *writer);

/** @brief Notifies the respective end (identified by function name) that the
 * provided end is waiting for a message to continue processing.
 *
 * The idea is to hash out the specifics in an Erlang-level communication
 * protocol, this merely provides an out-of-band way to say that further
 * communication is required, which can be very helpful when implementing
 * backpressure mechanisms. */
void erts_io_stream_notify_writer(ErtsIOStreamReader *reader);
/** @copydoc erts_io_stream_notify_reader */
void erts_io_stream_notify_reader(ErtsIOStreamWriter *writer);

/** @brief Provides a consistent view of the stream's underlying \c ErtsIOQueue
 * to a callback, which may call \c erts_ioq_deq to remove data from the
 * stream when necessary. */
int erts_io_stream_read(ErtsIOStreamReader *reader,
                        void (*callback)(ErtsIOQueue *queue, void *data),
                        void *data);

/** @brief Writes the given IO vector into the stream, optionally skipping the
 * first \c skip bytes. */
int erts_io_stream_write(ErtsIOStreamWriter *writer,
                         ErtsIOVec *vec,
                         Uint skip);

#endif
