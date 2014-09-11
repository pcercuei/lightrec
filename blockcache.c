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

#include "debug.h"
#include "lightrec.h"

/* Must be power of two */
#define LUT_SIZE 0x4000

static struct block * block_lut[LUT_SIZE];

struct block * lightrec_find_block(u32 pc)
{
	struct block *block = block_lut[(pc >> 2) & (LUT_SIZE - 1)];
	if (block && block->pc == pc)
		return block;
	else
		return NULL;
}

void lightrec_register_block(struct block *block)
{
	struct block *old = block_lut[(block->pc >> 2) & (LUT_SIZE - 1)];
	if (old && old->pc != block->pc)
		lightrec_free_block(old);
	block_lut[(block->pc >> 2) & (LUT_SIZE - 1)] = block;
}

void lightrec_free_block_cache(void)
{
	unsigned int i;

	for (i = 0; i < LUT_SIZE; i++)
		if (block_lut[i])
			lightrec_free_block(block_lut[i]);
}
