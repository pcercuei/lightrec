/*
 * Copyright (C) 2015 Paul Cercueil <paul@crapouillou.net>
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
#include "lightrec-private.h"

#include <stdlib.h>

/* Must be power of two */
#define TINY_LUT_SIZE 0x100
#define LUT_SIZE 0x40000

struct blockcache {
	struct block * tiny_lut[TINY_LUT_SIZE];
	struct block * lut[LUT_SIZE];
};

struct block * lightrec_find_block(struct blockcache *cache, u32 pc)
{
	struct block *block;

	pc = kunseg(pc);

	block = cache->tiny_lut[(pc >> 2) & (TINY_LUT_SIZE - 1)];
	if (likely(block && block->kunseg_pc == pc))
		return block;

	block = cache->lut[(pc >> 2) & (LUT_SIZE - 1)];
	if (block && block->kunseg_pc == pc) {
		cache->tiny_lut[(pc >> 2) & (TINY_LUT_SIZE - 1)] = block;
		return block;
	} else {
		return NULL;
	}
}

void lightrec_register_block(struct blockcache *cache, struct block *block)
{
	u32 pc = block->kunseg_pc;
	struct block *old = cache->lut[(pc >> 2) & (LUT_SIZE - 1)];
	if (old && old != block) {
		DEBUG("Freeing old block at pc 0x%x\n", old->pc);
		lightrec_free_block(old);
	}

	cache->lut[(pc >> 2) & (LUT_SIZE - 1)] = block;
	cache->tiny_lut[(pc >> 2) & (TINY_LUT_SIZE - 1)] = block;
}

void lightrec_unregister_block(struct blockcache *cache, struct block *block)
{
	u32 pc = block->kunseg_pc;
	struct block *old = cache->lut[(pc >> 2) & (LUT_SIZE - 1)];

	if (old) {
		if (old != block) {
			ERROR("Block at PC 0x%x is not in cache\n", block->pc);
			return;
		}

		cache->lut[(pc >> 2) & (LUT_SIZE - 1)] = NULL;
		cache->tiny_lut[(pc >> 2) & (TINY_LUT_SIZE - 1)] = NULL;
	}
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

u32 calculate_block_hash(const struct block *block)
{
	struct lightrec_state *state = block->state;
	u32 offset, count, hash = 0;

	if (block->kunseg_pc < 0x200000) {
		offset = block->kunseg_pc >> state->page_shift;
		count = (block->length + (1 << state->page_shift) - 1)
			>> state->page_shift;

		while (count--)
			hash += state->invalidation_table[offset++];
	}

	return hash;
}

bool lightrec_block_is_outdated(struct block *block)
{
	return block->hash != calculate_block_hash(block);
}
