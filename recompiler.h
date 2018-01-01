/*
 * Copyright (C) 2018 Paul Cercueil <paul@crapouillou.net>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 */

#ifndef __RECOMPILER_H__
#define __RECOMPILER_H__

#include "blockcache.h"

struct lightrec_state;
struct recompiler;

struct recompiler * lightrec_recompiler_init(struct lightrec_state *state);
void lightrec_free_recompiler(struct recompiler *rec);

void lightrec_request_recompile(struct recompiler *rec, u32 pc);

#endif /* __RECOMPILER_H__ */
