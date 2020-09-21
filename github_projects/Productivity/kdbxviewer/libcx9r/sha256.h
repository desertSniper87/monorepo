/* Cryptkeyper is
 *
 *     Copyright (C) 2013 Jonas Hagmar (jonas.hagmar@gmail.com)
 *
 * This file is part of cryptkeyper.
 *
 * Cryptkeyper is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 * Cryptkeyper is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with cryptkeyper. If not, see <http://www.gnu.org/licenses/>.
 */

// This a wrapper for libraries containing SHA256 implementations.
// Currently only libgcrypt supported.
#ifndef CX9R_SHA256_H
#define CX9R_SHA256_H

#include <cx9r.h>
#include "../config.h"

#define CX9R_SHA256_HASH_LENGTH 32

#ifdef GCRYPT_WITH_SHA256
#   include <gcrypt.h>
    typedef gcry_md_hd_t cx9r_sha256_ctx;
#else
#error No libgcrypt support for sha256
#endif

#include <stdint.h>

cx9r_err cx9r_sha256_init(cx9r_sha256_ctx *ctx);
cx9r_err cx9r_sha256_process(cx9r_sha256_ctx *ctx, uint8_t *buffer, size_t length);
cx9r_err cx9r_sha256_close(cx9r_sha256_ctx *ctx, uint8_t *hash);
cx9r_err cx9r_sha256_hash_buffer(uint8_t *hash, uint8_t *buffer, size_t length);

#endif

