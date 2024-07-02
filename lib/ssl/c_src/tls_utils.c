
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

#include <string.h>

size_t tls_record_copy_vec(SysIOVecPosition *pos, size_t size, byte *data) {
    size_t copied = 0;

    ASSERT(pos->vec_offset < pos->vec_len);
    ASSERT(pos->block_offset < pos->vec[pos->vec_offset].iov_len);

    while (copied < size && pos->vec_offset < pos->vec_len) {
        size_t block_size, copy_size;
        byte *block_start;

        block_start = (byte *)pos->vec[pos->vec_offset].iov_base;
        block_size = pos->vec[pos->vec_offset].iov_len;

        copy_size = MIN(size - copied, block_size) - pos->block_offset;
        memcpy(&data[copied], &block_start[pos->block_offset], copy_size);
        copied += copy_size;

        if (copied == size && block_size > (pos->block_offset + copy_size)) {
            pos->block_offset += copy_size;
            break;
        }

        pos->block_offset = 0;
        pos->vec_offset++;
    }

    return copied;
}
