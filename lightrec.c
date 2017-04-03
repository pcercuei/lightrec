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
#include "disassembler.h"
#include "emitter.h"
#include "lightrec.h"
#include "regcache.h"
#include "optimizer.h"

#include <lightning.h>
#include <limits.h>
#include <stddef.h>
#include <string.h>

#define GENMASK(h, l) \
	(((~0UL) << (l)) & (~0UL >> (__WORDSIZE - 1 - (h))))

static void __segfault_cb(struct lightrec_state *state, u32 addr)
{
	lightrec_set_exit_flags(state, LIGHTREC_EXIT_SEGFAULT);
	ERROR("Segmentation fault in recompiled code: invalid "
			"load/store at address 0x%08x\n", addr);
}

static u32 kunseg(u32 addr)
{
	if (unlikely(addr >= 0xa0000000))
		return addr - 0xa0000000;
	else if (addr >= 0x80000000)
		return addr - 0x80000000;
	else
		return addr;
}

static u32 lightrec_rw_ops(struct lightrec_state *state,
		const struct opcode *op, const struct lightrec_mem_map_ops *ops,
		u32 addr, u32 data)
{
	switch (op->i.op) {
	case OP_SB:
		ops->sb(state, op, addr, (u8) data);
		return 0;
	case OP_SH:
		ops->sh(state, op, addr, (u16) data);
		return 0;
	case OP_SWL:
	case OP_SWR:
	case OP_SW:
		ops->sw(state, op, addr, data);
		return 0;
	case OP_LB:
		return (s32) (s8) ops->lb(state, op, addr);
	case OP_LBU:
		return ops->lb(state, op, addr);
	case OP_LH:
		return (s32) (s16) ops->lh(state, op, addr);
	case OP_LHU:
		return ops->lh(state, op, addr);
	case OP_LW:
	default:
		return ops->lw(state, op, addr);
	}
}

static void lightrec_invalidate_map(const struct lightrec_mem_map *map,
		u32 addr, u32 len, u32 current_cycle)
{
	struct lightrec_mem_map_priv *priv;
	u32 offset, count;

	while (map->mirror_of)
		map = map->mirror_of;

	if (!(map->flags & MAP_IS_RWX))
		return;

	priv = container_of(map, struct lightrec_mem_map_priv, map);
	offset = (addr - map->pc) >> priv->page_shift;
	count = (len + (1 << priv->page_shift) - 1) >> priv->page_shift;

	while (count--)
		priv->invalidation_table[offset++] = current_cycle;
}

u32 lightrec_rw(struct lightrec_state *state,
		const struct opcode *op, u32 addr, u32 data)
{
	unsigned int i;
	u32 kaddr;

	addr += (s16) op->i.imm;
	kaddr = kunseg(addr);

	for (i = 0; i < state->nb_maps; i++) {
		const struct lightrec_mem_map *map = &state->mem_map[i].map;
		const struct lightrec_mem_map_ops *ops = map->ops;
		u32 shift, mem_data, mask, pc = map->pc;
		uintptr_t new_addr;

		if (kaddr < pc || kaddr >= pc + map->length)
			continue;

		if (unlikely(ops))
			return lightrec_rw_ops(state, op, ops, addr, data);

		while (map->mirror_of)
			map = map->mirror_of;

		new_addr = (uintptr_t) map->address + (kaddr - pc);

		switch (op->i.op) {
		case OP_SB:
			*(u8 *) new_addr = (u8) data;
			lightrec_invalidate_map(map, kaddr, 1,
					state->current_cycle);
			return 0;
		case OP_SH:
			*(u16 *) new_addr = (u16) data;
			lightrec_invalidate_map(map, kaddr, 2,
					state->current_cycle);
			return 0;
		case OP_SWL:
			shift = kaddr & 3;
			mem_data = *(u32 *)(new_addr & ~3);
			mask = GENMASK(31, (shift + 1) * 8);

			*(u32 *)(new_addr & ~3) = (data >> ((3 - shift) * 8))
				| (mem_data & mask);
			lightrec_invalidate_map(map, kaddr & ~0x3, 4,
					state->current_cycle);
			return 0;
		case OP_SWR:
			shift = kaddr & 3;
			mem_data = *(u32 *)(new_addr & ~3);
			mask = (1 << (shift * 8)) - 1;

			*(u32 *)(new_addr & ~3) = (data << (shift * 8))
				| (mem_data & mask);
			lightrec_invalidate_map(map, kaddr & ~0x3, 4,
					state->current_cycle);
			return 0;
		case OP_SW:
			*(u32 *) new_addr = data;
			lightrec_invalidate_map(map, kaddr, 4,
					state->current_cycle);
			return 0;
		case OP_SWC2:
			if (!state->cop_ops || !state->cop_ops->mfc) {
				WARNING("Missing MFC callback!\n");
				return 0;
			}

			*(u32 *) new_addr = state->cop_ops->mfc(
					state, 2, op->i.rt);
			lightrec_invalidate_map(map, kaddr, 4,
					state->current_cycle);
			return 0;
		case OP_LB:
			return (s32) *(s8 *) new_addr;
		case OP_LBU:
			return *(u8 *) new_addr;
		case OP_LH:
			return (s32) *(s16 *) new_addr;
		case OP_LHU:
			return *(u16 *) new_addr;
		case OP_LWL:
			shift = kaddr & 3;
			mem_data = *(u32 *)(new_addr & ~3);
			mask = (1 << (24 - shift * 8)) - 1;

			return (data & mask) | (mem_data << (24 - shift * 8));
		case OP_LWR:
			shift = kaddr & 3;
			mem_data = *(u32 *)(new_addr & ~3);
			mask = GENMASK(31, 32 - shift * 8);

			return (data & mask) | (mem_data >> (shift * 8));
		case OP_LWC2:
			if (!state->cop_ops || !state->cop_ops->mtc) {
				WARNING("Missing MFC callback!\n");
				return 0;
			}

			state->cop_ops->mtc(state, 2,
					op->i.rt, *(u32 *) new_addr);
			return 0;
		case OP_LW:
		default:
			return *(u32 *) new_addr;
		}
	}

	__segfault_cb(state, addr);
	return 0;
}

static struct lightrec_mem_map_priv * find_map(
		struct lightrec_state *state, u32 pc)
{
	struct lightrec_mem_map_priv *map = state->mem_map;
	unsigned int i;

	for (i = 0; i < state->nb_maps; i++)
		if (pc >= map[i].map.pc &&
				pc < map[i].map.pc + map[i].map.length)
			return &map[i];

	return NULL;
}

static struct block * generate_wrapper_block(struct lightrec_state *state)
{
	struct block *block;
	jit_state_t *_jit;
	jit_node_t *addr;
	unsigned int i;

	block = malloc(sizeof(*block));
	if (!block)
		goto err_no_mem;

	_jit = jit_new_state();
	if (!_jit)
		goto err_free_block;

	jit_name("wrapper");
	jit_note(__FILE__, __LINE__);

	jit_prolog();
	jit_frame(256);

	jit_getarg(JIT_R0, jit_arg());

	/* Force all callee-saved registers to be pushed on the stack */
	for (i = 0; i < NUM_REGS; i++)
		jit_movr(JIT_V(i), JIT_V(i));

	/* Pass lightrec_state structure to blocks, using the last callee-saved
	 * register that Lightning provides */
	jit_movi(LIGHTREC_REG_STATE, (intptr_t) state);

	/* Call the block's code */
	jit_jmpr(JIT_R0);

	jit_note(__FILE__, __LINE__);

	/* The block will not return, but jump right here */
	addr = jit_indirect();
	jit_epilog();

	block->state = state;
	block->_jit = _jit;
	block->function = jit_emit();
	block->opcode_list = NULL;

	/* When exiting, the recompiled code will jump to that address */
	state->end_of_block = (uintptr_t) jit_address(addr);

#if (LOG_LEVEL >= DEBUG_L)
	DEBUG("Wrapper block:\n");
	jit_disassemble();
#endif

	/* We're done! */
	jit_clear_state();
	return block;

err_free_block:
	free(block);
err_no_mem:
	ERROR("Unable to compile wrapper: Out of memory\n");
	return NULL;
}

struct block * lightrec_recompile_block(struct lightrec_state *state, u32 pc)
{
	struct opcode *elm, *list;
	struct block *block;
	jit_state_t *_jit;
	bool skip_next = false;
	const u32 *code;
	u32 addr, kunseg_pc = kunseg(pc);
	const struct lightrec_mem_map_priv *pmap = find_map(state, kunseg_pc);
	const struct lightrec_mem_map *map;

	if (!pmap)
		return NULL;

	map = &pmap->map;
	addr = kunseg_pc - map->pc;

	while (map->mirror_of)
		map = map->mirror_of;

	code = map->address + addr;

	block = malloc(sizeof(*block));
	if (!block)
		goto err_no_mem;

	list = lightrec_disassemble(code, &block->length);
	if (!list)
		goto err_free_block;

	_jit = jit_new_state();
	if (!_jit)
		goto err_free_list;

	lightrec_regcache_reset(state->reg_cache);

	block->pc = pc;
	block->kunseg_pc = map->pc + addr;
	block->state = state;
	block->_jit = _jit;
	block->opcode_list = list;
	block->cycles = 0;
	block->code = code;
	block->map = pmap;
	block->hash = calculate_block_hash(block);

	lightrec_optimize(list);

	jit_prolog();
	jit_tramp(256);

	for (elm = list; elm; elm = SLIST_NEXT(elm, next)) {
		int ret;

		block->cycles += lightrec_cycles_of_opcode(elm);

		if (skip_next) {
			skip_next = false;
		} else if (elm->opcode) {
			ret = lightrec_rec_opcode(block, elm, pc);
			skip_next = ret == SKIP_DELAY_SLOT;
		}

		if (likely(!(elm->flags & LIGHTREC_SKIP_PC_UPDATE)))
			pc += 4;
	}

	jit_ret();
	jit_epilog();

	block->function = jit_emit();

#if (LOG_LEVEL >= DEBUG_L)
	DEBUG("Recompiling block at PC: 0x%x\n", block->pc);
	lightrec_print_disassembly(block);
	DEBUG("Lightrec generated:\n");
	jit_disassemble();
#endif
	jit_clear_state();
	return block;

err_free_list:
	lightrec_free_opcode_list(list);
err_free_block:
	free(block);
err_no_mem:
	ERROR("Unable to recompile block: Out of memory\n");
	return NULL;
}

u32 lightrec_execute(struct lightrec_state *state, u32 pc, u32 target_cycle)
{
	void (*func)(void *) = (void (*)(void *)) state->wrapper->function;
	struct block *block = lightrec_find_block(state->block_cache, pc);

	if (block && lightrec_block_is_outdated(block)) {
		DEBUG("Block at PC 0x%08x is outdated!\n", block->pc);

		lightrec_unregister_block(state->block_cache, block);
		lightrec_free_block(block);
		block = NULL;
	}

	if (!block) {
		block = lightrec_recompile_block(state, pc);
		if (!block) {
			ERROR("Unable to recompile block at PC 0x%x\n", pc);
			return pc;
		}

		lightrec_register_block(state->block_cache, block);
	}

	state->exit_flags = LIGHTREC_EXIT_NORMAL;
	state->current = block;

	/* Handle the cycle counter overflowing */
	if (unlikely(target_cycle < state->current_cycle))
		target_cycle = UINT_MAX;

	state->target_cycle = target_cycle;

	func((void *) block->function);
	return state->next_pc;
}

u32 lightrec_execute_one(struct lightrec_state *state, u32 pc)
{
	return lightrec_execute(state, pc, state->current_cycle);
}

void lightrec_free_block(struct block *block)
{
	lightrec_free_opcode_list(block->opcode_list);
	_jit_destroy_state(block->_jit);
	free(block);
}

struct lightrec_state * lightrec_init(char *argv0,
		const struct lightrec_mem_map *map, size_t nb,
		const struct lightrec_cop_ops *cop_ops)
{
	struct lightrec_state *state;
	unsigned int i;

	init_jit(argv0);

	state = calloc(1, sizeof(*state));
	if (!state)
		goto err_finish_jit;

	state->block_cache = lightrec_blockcache_init();
	if (!state->block_cache)
		goto err_free_state;

	state->reg_cache = lightrec_regcache_init();
	if (!state->reg_cache)
		goto err_free_block_cache;

	state->mem_map = calloc(nb, sizeof(*state->mem_map));
	if (!state->mem_map)
		goto err_free_reg_cache;

	state->nb_maps = nb;

	for (i = 0; i < nb; i++) {
		struct lightrec_mem_map_priv *map_priv = &state->mem_map[i];

		map_priv->map = map[i];

		if (!(map_priv->map.flags & MAP_IS_RWX))
			continue;

		/* TODO: calculate the best page shift */
		map_priv->page_shift = 9;

		map_priv->invalidation_table = calloc(
				(map->length >> map_priv->page_shift) + 1,
				sizeof(u32));
		if (!map_priv->invalidation_table)
			goto err_free_invalidation_tables;
	}

	state->cop_ops = cop_ops;
	state->wrapper = generate_wrapper_block(state);

	return state;

err_free_invalidation_tables:
	for (i = 0; i < nb; i++)
		free(state->mem_map[i].invalidation_table);
	free(state->mem_map);
err_free_reg_cache:
	lightrec_free_regcache(state->reg_cache);
err_free_block_cache:
	lightrec_free_block_cache(state->block_cache);
err_free_state:
	free(state);
err_finish_jit:
	finish_jit();
	return NULL;
}

void lightrec_destroy(struct lightrec_state *state)
{
	unsigned int i;

	lightrec_free_regcache(state->reg_cache);
	lightrec_free_block_cache(state->block_cache);
	lightrec_free_block(state->wrapper);
	finish_jit();

	for (i = 0; i < state->nb_maps; i++)
		free(state->mem_map[i].invalidation_table);
	free(state->mem_map);
	free(state);
}

void lightrec_invalidate(struct lightrec_state *state, u32 addr, u32 len)
{
	unsigned int i;

	addr = kunseg(addr);

	for (i = 0; i < state->nb_maps; i++) {
		const struct lightrec_mem_map *map = &state->mem_map[i].map;
		u32 offset, count;

		if (addr >= map->pc && addr <= map->pc + map->length) {
			lightrec_invalidate_map(map, addr, len,
					state->current_cycle);
			break;
		}
	}
}

void lightrec_set_exit_flags(struct lightrec_state *state, u32 flags)
{
	state->exit_flags |= flags;
}

u32 lightrec_exit_flags(struct lightrec_state *state)
{
	return state->exit_flags;
}

void lightrec_dump_registers(struct lightrec_state *state, u32 regs[34])
{
	memcpy(regs, state->native_reg_cache, sizeof(state->native_reg_cache));
}

void lightrec_restore_registers(struct lightrec_state *state, u32 regs[34])
{
	memcpy(state->native_reg_cache, regs, sizeof(state->native_reg_cache));
}

u32 lightrec_current_cycle_count(const struct lightrec_state *state,
		const struct opcode *op)
{
	u32 cycles = state->current_cycle;

	if (op)
		cycles += lightrec_cycles_of_block(state->current, op);

	return cycles;
}

void lightrec_reset_cycle_count(struct lightrec_state *state, u32 cycles)
{
	state->current_cycle = cycles;
}
