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

#include "blockcache.h"
#include "debug.h"
#include "lightrec.h"

#include <stdlib.h>

/* Must be power of two */
#define LUT_SIZE 0x4000

struct blockcache {
	struct block * lut[LUT_SIZE];
};

struct block * lightrec_find_block(struct blockcache *cache, u32 pc)
{
	struct block *block = cache->lut[(pc >> 2) & (LUT_SIZE - 1)];
	if (block && block->pc == pc)
		return block;
	else
		return NULL;
}

void lightrec_register_block(struct blockcache *cache, struct block *block)
{
	struct block *old = cache->lut[(block->pc >> 2) & (LUT_SIZE - 1)];
	if (old && old->pc != block->pc) {
		WARNING("Freeing old block at pc 0x%x\n", old->pc);
		lightrec_free_block(old);
	}

	cache->lut[(block->pc >> 2) & (LUT_SIZE - 1)] = block;
}

void lightrec_free_block_cache(struct blockcache *cache)
{
	unsigned int i;

	for (i = 0; i < LUT_SIZE; i++)
		if (cache->lut[i])
			lightrec_free_block(cache->lut[i]);
	free(cache);
}

struct blockcache * lightrec_blockcache_init(void)
{
	return calloc(1, sizeof(struct blockcache));
}
