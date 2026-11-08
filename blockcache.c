// SPDX-License-Identifier: LGPL-2.1-or-later
/*
 * Copyright (C) 2015-2021 Paul Cercueil <paul@crapouillou.net>
 */

#include "blockcache.h"
#include "debug.h"
#include "lightrec-private.h"
#include "memmanager.h"
#include "reaper.h"
#include "recompiler.h"

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

/*
 * The blocks are kept in a doubly-linked list ordered by their entry point.
 * To boost the access speed of the list, we use a LUT of anchor points.
 * The RAM and BIOS memories are cut in segments of (1 << LUT_SIZE_SHIFT) bytes;
 * in each of these segments, the anchor point is the block with the lowest PC.
 */
#define LUT_SIZE_SHIFT 8
#define LUT_SIZE ((RAM_SIZE + BIOS_SIZE) >> LUT_SIZE_SHIFT)

struct blockcache {
	struct lightrec_state *state;
	struct block *head;
	struct block *lut[LUT_SIZE];
};

static inline u16 pc_get_lut_entry(u32 pc)
{
	u16 entry;

	if (pc_in_bios(pc))
		entry = (RAM_SIZE + (pc & (BIOS_SIZE - 1))) >> LUT_SIZE_SHIFT;
	else
		entry = (pc & (RAM_SIZE - 1)) >> LUT_SIZE_SHIFT;

	return entry;
}

static inline u16 lightrec_get_lut_entry(const struct block *block)
{
	return pc_get_lut_entry(block->kunseg_pc);
}

static inline u16 lightrec_get_last_lut_entry(const struct block *block)
{
	return pc_get_lut_entry(block->kunseg_pc + (block->nb_ops - 1) * 4);
}

static struct block * blockcache_find_anchor(struct blockcache *cache, u16 entry)
{
	struct block *anchor = NULL;
	unsigned int i;

	for (i = entry; !anchor && i < LUT_SIZE; i++)
		anchor = cache->lut[i];

	for (i = entry; !anchor && i > 0; i--)
		anchor = cache->lut[i - 1];

	return anchor;
}

struct block * lightrec_find_block(struct blockcache *cache, u32 pc)
{
	struct block *block;
	u32 kunseg_pc = kunseg(pc);
	u16 entry = pc_get_lut_entry(pc);

	for (block = cache->lut[entry]; block; block = block->next) {
		if (block->kunseg_pc > kunseg_pc)
			break;

		if (block->kunseg_pc == kunseg_pc)
			return block;
	}

	return NULL;
}

struct block * lightrec_find_block_from_lut(struct blockcache *cache,
					    u16 tag, u32 addr_in_block)
{
	u16 entry = pc_get_lut_entry(addr_in_block);
	u32 kunseg_pc = kunseg(addr_in_block);
	struct block *block;

	for (block = cache->lut[entry]; block; block = block->next) {
		if (block->kunseg_pc > kunseg_pc)
			break;

		if ((u16)(block->kunseg_pc >> 2) == tag
		    && block->kunseg_pc + block->nb_ops * 4 > kunseg_pc)
			return block;
	}

	return NULL;
}

void remove_from_code_lut(struct blockcache *cache, struct block *block)
{
	struct lightrec_state *state = cache->state;
	u32 offset = lut_offset(block->pc);

	if (block->function) {
		memset(lut_address(state, offset), 0,
		       block->nb_ops * lut_elm_size(state));
	}
}

void lightrec_register_block(struct blockcache *cache, struct block *block)
{
	u16 last_entry = lightrec_get_last_lut_entry(block);
	u16 first_entry = lightrec_get_lut_entry(block);
	struct block *anchor, *prev, *next;
	u16 entry;

	anchor = blockcache_find_anchor(cache, first_entry);

	if (!anchor) {
		prev = NULL;
		next = NULL;
	} else if (anchor->kunseg_pc <= block->kunseg_pc) {
		while (anchor->next && anchor->next->kunseg_pc <= block->kunseg_pc)
			anchor = anchor->next;

		prev = anchor;
		next = anchor->next;
	} else {
		while (anchor->prev && anchor->prev->kunseg_pc >= block->kunseg_pc)
			anchor = anchor->prev;

		prev = anchor->prev;
		next = anchor;
	}

	block->prev = prev;
	block->next = next;

	if (prev)
		prev->next = block;
	else
		cache->head = block;
	if (next)
		next->prev = block;

	for (entry = first_entry; entry <= last_entry; entry++) {
		if (!cache->lut[entry] || block->kunseg_pc < cache->lut[entry]->kunseg_pc)
			cache->lut[entry] = block;
	}

	remove_from_code_lut(cache, block);
}

void lightrec_unregister_block(struct blockcache *cache, struct block *block)
{
	u16 last_entry = lightrec_get_last_lut_entry(block);
	u16 first_entry = lightrec_get_lut_entry(block);
	struct block *first, *b;
	u16 entry;

	if (unlikely(!block->prev && cache->head != block)) {
		pr_err("Block at "PC_FMT" is not in cache\n", block->pc);
		return;
	}

	for (entry = first_entry; entry <= last_entry; entry++) {
		if (cache->lut[entry] != block)
			continue;

		first = NULL;

		for (b = block->next; b; b = b->next) {
			if (lightrec_get_lut_entry(b) > entry)
				break;

			if (lightrec_get_last_lut_entry(b) >= entry) {
				first = b;
				break;
			}
		}

		cache->lut[entry] = first;
	}

	if (block->prev)
		block->prev->next = block->next;
	else
		cache->head = block->next;

	if (block->next)
		block->next->prev = block->prev;

	block->prev = NULL;
	block->next = NULL;
}

static bool lightrec_block_is_old(const struct lightrec_state *state,
				  const struct block *block)
{
	u32 diff = state->current_cycle - block->precompile_date;

	return diff > (1 << 27); /* About 4 seconds */
}

static void lightrec_free_blocks(struct blockcache *cache,
				 const struct block *except, bool all)
{
	struct lightrec_state *state = cache->state;
	struct block *block, *next;
	bool outdated = all;
	u8 old_flags;

	for (block = cache->head; block; block = next) {
		next = block->next;

		if (block == except)
			continue;

		if (!all) {
			outdated = lightrec_block_is_old(state, block) ||
				lightrec_block_is_outdated(state, block);
		}

		if (!outdated)
			continue;

		old_flags = block_set_flags(block, BLOCK_IS_DEAD);

		if (!(old_flags & BLOCK_IS_DEAD)) {
			if (ENABLE_THREADED_COMPILER)
				lightrec_recompiler_remove(state->rec, block);

			pr_debug("Freeing outdated block at "PC_FMT"\n", block->pc);
			remove_from_code_lut(cache, block);
			lightrec_unregister_block(cache, block);
			lightrec_free_block(state, block);
		}
	}
}

void lightrec_remove_outdated_blocks(struct blockcache *cache,
				     const struct block *except)
{
	pr_info("Running out of code space. Cleaning block cache...\n");

	lightrec_free_blocks(cache, except, false);
}

void lightrec_free_all_blocks(struct blockcache *cache)
{
	lightrec_free_blocks(cache, NULL, true);
}

void lightrec_free_block_cache(struct blockcache *cache)
{
	lightrec_free_all_blocks(cache);
	lightrec_free(cache->state, MEM_FOR_LIGHTREC, sizeof(*cache), cache);
}

struct blockcache * lightrec_blockcache_init(struct lightrec_state *state)
{
	struct blockcache *cache;

	cache = lightrec_calloc(state, MEM_FOR_LIGHTREC, sizeof(*cache));
	if (!cache)
		return NULL;

	cache->state = state;

	return cache;
}

u32 lightrec_calculate_block_hash(const struct block *block)
{
	const u32 *code = block->code;
	u32 hash = 0xffffffff;
	unsigned int i;

	/* Jenkins one-at-a-time hash algorithm */
	for (i = 0; i < block->nb_ops; i++) {
		hash += *code++;
		hash += (hash << 10);
		hash ^= (hash >> 6);
	}

	hash += (hash << 3);
	hash ^= (hash >> 11);
	hash += (hash << 15);

	return hash;
}

static void lightrec_reset_lut_offset(struct lightrec_state *state, void *d)
{
	u32 pc = (u32)(uintptr_t) d;
	struct block *block;
	void *addr;

	block = lightrec_find_block(state->block_cache, pc);
	if (!block)
		return;

	if (block_has_flag(block, BLOCK_IS_DEAD))
		return;

	addr = block->function ?: state->get_next_block;
	lut_write(state, lut_offset(pc), addr);
}

bool lightrec_block_is_outdated(struct lightrec_state *state, struct block *block)
{
	u32 offset = lut_offset(block->pc);
	bool outdated;

	if (lut_read(state, offset))
		return false;

	outdated = block->hash != lightrec_calculate_block_hash(block);
	if (likely(!outdated)) {
		/* The block was marked as outdated, but the content is still
		 * the same */

		if (ENABLE_THREADED_COMPILER) {
			/*
			 * When compiling a block that covers ours, the threaded
			 * compiler will set the LUT entries of the various
			 * entry points. Therefore we cannot write the LUT here,
			 * as we would risk overwriting the new entry points.
			 * Leave it to the reaper to re-install the LUT entries.
			 */

			lightrec_reaper_add(state->reaper,
					    lightrec_reset_lut_offset,
					    (void *)(uintptr_t) block->pc);
		} else if (block->function) {
			lut_write(state, offset, block->function);
		} else {
			lut_write(state, offset, state->get_next_block);
		}
	}

	return outdated;
}
