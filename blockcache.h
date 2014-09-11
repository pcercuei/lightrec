/*
 * Copyright (C) 2014 Paul Cercueil <paul@crapouillou.net>
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

#ifndef __BLOCKCACHE_H__
#define __BLOCKCACHE_H__

#include "lightrec.h"

struct block * lightrec_find_block(u32 pc);
void lightrec_register_block(struct block *block);

void lightrec_free_block_cache(void);

#endif /* __BLOCKCACHE_H__ */
