/*
 * Copyright (c) 2022 Apple Inc. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY APPLE INC. ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL APPLE INC. OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "pas_config.h"

#if LIBPAS_ENABLED

#include "bmalloc_heap_config.h"
#include "bmalloc_heap_inlines.h"
#include "pas_darwin_spi.h"
#include "pas_malloc_stack_logging.h"
#include <stdlib.h>

PAS_BEGIN_EXTERN_C;

pas_msl_is_enabled_flag pas_msl_is_enabled_flag_value = pas_msl_is_enabled_flag_indeterminate;
static void compute_msl_status(void)
{
    if (!!getenv("MallocStackLogging"))
        pas_msl_is_enabled_flag_value = pas_msl_is_enabled_flag_enabled;
    else
        pas_msl_is_enabled_flag_value = pas_msl_is_enabled_flag_disabled;
}

bool pas_compute_msl_is_enabled(void)
{
    static pthread_once_t key = PTHREAD_ONCE_INIT;
    pthread_once(&key, compute_msl_status);
    return pas_msl_is_enabled_flag_value == pas_msl_is_enabled_flag_enabled;
}

PAS_END_EXTERN_C;

#endif /* LIBPAS_ENABLED */
