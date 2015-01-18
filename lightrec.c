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
#include "emitter.h"
#include "lightrec.h"
#include "recompiler.h"
#include "regcache.h"

#include <lightning.h>
#include <stddef.h>
#include <string.h>

#if JIT_V_NUM < 4
#error "At least 4 callee-saved registers are needed"
#endif

static u32 lightrec_rw(struct lightrec_state *state,
		union opcode op, u32 addr, u32 data)
{
	struct lightrec_mem_map *map = state->mem_map;
	unsigned int i;

	addr += (s16) op.i.imm;

	for (i = 0; i < state->nb_maps; i++) {
		if (addr >= map[i].pc && addr < map[i].pc + map[i].length) {
			uintptr_t new_addr = (uintptr_t) map[i].address +
				(addr - map[i].pc);
			struct lightrec_mem_map_ops *ops = map[i].ops;

			switch (op.i.op) {
			case OP_SB:
				if (unlikely(ops && ops->sb))
					ops->sb(state, addr, (u8) data);
				else
					*(u8 *) new_addr = (u8) data;
				return 0;
			case OP_SH:
				if (unlikely(ops && ops->sh))
					ops->sh(state, addr, (u16) data);
				else
					*(u16 *) new_addr = (u16) data;
				return 0;
			case OP_SW:
			case OP_SWL:
			case OP_SWR:
				if (unlikely(ops && ops->sw))
					ops->sw(state, addr, data);
				else
					*(u32 *) new_addr = data;
				return 0;
			case OP_LB:
				if (unlikely(ops && ops->lb))
					return (s32) (s8) ops->lb(state, addr);
				else
					return (s32) *(s8 *) new_addr;
			case OP_LBU:
				if (unlikely(ops && ops->lb))
					return ops->lb(state, addr);
				else
					return *(u8 *) new_addr;
			case OP_LH:
				if (unlikely(ops && ops->lh))
					return (s32) (s16) ops->lh(state, addr);
				else
					return (s32) *(s16 *) new_addr;
			case OP_LHU:
				if (unlikely(ops && ops->lh))
					return ops->lh(state, addr);
				else
					return *(u16 *) new_addr;
			case OP_LW:
			case OP_LWL:
			case OP_LWR:
			default:
				if (unlikely(ops && ops->lw))
					return ops->lw(state, addr);
				else
					return *(u32 *) new_addr;
			}
		}
	}

	return 0;
}

static const u32 * find_code_address(struct lightrec_state *state, u32 pc)
{
	struct lightrec_mem_map *map = state->mem_map;
	unsigned int i;

	for (i = 0; i < state->nb_maps; i++) {
		if (pc >= map[i].pc && pc < map[i].pc + map[i].length)
			return map[i].address + (pc - map[i].pc);
	}

	return NULL;
}

static void __segfault_cb(unsigned long addr)
{
	/* TODO: Handle the segmentation fault? */
	ERROR("Segmentation fault in recompiled code! Addr=0x%lx\n", addr);
}

static struct block * generate_address_lookup_block(
		struct lightrec_state *state, unsigned int nb_maps)
{
	struct block *block;
	jit_state_t *_jit;
	jit_node_t *loop_top, *addr, *addr2, *addr3, *to_end;

	block = malloc(sizeof(*block));
	if (!block)
		goto err_no_mem;

	_jit = jit_new_state();
	if (!_jit)
		goto err_free_block;

	jit_prolog();
	jit_getarg(JIT_V2, jit_arg());

	jit_name("address_lookup");
	jit_note(__FILE__, __LINE__);

	/* Make the LIGHTREC_REG_STATE register point to lightrec_state->mem_map
	 * just for the algorithm, to save one register */
	jit_addi(LIGHTREC_REG_STATE, LIGHTREC_REG_STATE,
			offsetof(struct lightrec_state, mem_map));

	/* Make JIT_V0 point to the last map */
	jit_addi(JIT_V0, LIGHTREC_REG_STATE, (nb_maps - 1) *
			sizeof(struct lightrec_mem_map));

	loop_top = jit_label();

	/* Test if addr >= curr_map->pc */
	jit_ldxi_i(JIT_R0, JIT_V0, offsetof(struct lightrec_mem_map, pc));
	addr = jit_bltr_u(JIT_V2, JIT_R0);

	/* Test if addr < curr_map->pc + curr_map->length */
	jit_ldxi_i(JIT_V1, JIT_V0, offsetof(struct lightrec_mem_map, length));
	jit_addr(JIT_V1, JIT_R0, JIT_V1);
	addr2 = jit_bger_u(JIT_V2, JIT_V1);

	/* Found: calculate address and jump to end */
	jit_ldxi(JIT_V1, JIT_V0, offsetof(struct lightrec_mem_map, address));
	jit_subr(JIT_R0, JIT_V2, JIT_R0);
	jit_addr(JIT_R0, JIT_R0, JIT_V1);
	to_end = jit_jmpi();

	jit_patch(addr);
	jit_patch(addr2);

	/* End of loop: test JIT_V0 == LIGHTREC_REG_STATE, continue if true */
	jit_subi(JIT_V0, JIT_V0, sizeof(struct lightrec_mem_map));
	addr3 = jit_bger_u(JIT_V0, LIGHTREC_REG_STATE);
	jit_patch_at(addr3, loop_top);

	/* TODO: Handle segfault */
	jit_calli(&__segfault_cb);

	jit_patch(to_end);

	/* And return the address to the caller */
	jit_retr(JIT_R0);
	jit_epilog();

	block->state = state;
	block->_jit = _jit;
	block->function = jit_emit();
	block->opcode_list = NULL;
#if (LOG_LEVEL >= DEBUG_L)
	jit_disassemble();
#endif

	jit_clear_state();
	return block;

err_free_block:
	free(block);
err_no_mem:
	ERROR("Unable to compile wrapper: Out of memory\n");
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
	jit_movi(LIGHTREC_REG_STATE, state);

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
	struct opcode_list *elm, *list;
	struct block *block;
	jit_state_t *_jit;
	bool skip_next = false;
	const u32 *code = find_code_address(state, pc);
	if (!code)
		return NULL;

	block = malloc(sizeof(*block));
	if (!block)
		goto err_no_mem;

	list = lightrec_disassemble(code);
	if (!list)
		goto err_free_block;

	_jit = jit_new_state();
	if (!_jit)
		goto err_free_list;

	lightrec_regcache_reset();

	block->pc = pc;
	block->state = state;
	block->_jit = _jit;
	block->opcode_list = list;
	block->cycles = 0;

	jit_prolog();
	jit_tramp(256);

	for (elm = list; elm; elm = SLIST_NEXT(elm, next), pc += 4) {
		int ret;

		block->cycles += lightrec_cycles_of_opcode(elm->opcode);

		if (skip_next) {
			skip_next = false;
			continue;
		}

		/* Don't recompile NOPs */
		if (!elm->opcode.opcode)
			continue;

		ret = lightrec_rec_opcode(block, elm->opcode, pc);
		skip_next = ret == SKIP_DELAY_SLOT;
	}

	jit_ret();
	jit_epilog();

	block->function = jit_emit();

#if (LOG_LEVEL >= DEBUG_L)
	DEBUG("Recompiling block at PC: 0x%x\n", block->pc);
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

u32 lightrec_execute(struct lightrec_state *state, u32 pc)
{
	void (*func)(void *) = (void (*)(void *)) state->wrapper->function;
	struct block *block = lightrec_find_block(state->block_cache, pc);

	if (!block) {
		block = lightrec_recompile_block(state, pc);
		if (!block) {
			ERROR("Unable to recompile block at PC 0x%x\n", pc);
			return pc;
		}

		lightrec_register_block(state->block_cache, block);
	}

	state->block_exit_flags = LIGHTREC_EXIT_NORMAL;
	state->block_exit_cycles = 0;

	func((void *) block->function);
	return state->next_pc;
}

void lightrec_free_block(struct block *block)
{
	lightrec_free_opcode_list(block->opcode_list);
	_jit_destroy_state(block->_jit);
	free(block);
}

struct lightrec_state * lightrec_init(char *argv0,
		struct lightrec_mem_map *map, unsigned int nb,
		const struct lightrec_cop_ops *cop_ops)
{
	struct lightrec_state *state;

	init_jit(argv0);

	state = calloc(1, sizeof(*state) + nb * sizeof(*map));
	state->block_cache = lightrec_blockcache_init();

	state->nb_maps = nb;
	memcpy(state->mem_map, map, nb * sizeof(*map));

	state->cop_ops = cop_ops;
	state->rw_op = lightrec_rw;

	state->wrapper = generate_wrapper_block(state);

	state->addr_lookup_block = generate_address_lookup_block(state, nb);
	state->addr_lookup = state->addr_lookup_block->function;
	return state;
}

void lightrec_destroy(struct lightrec_state *state)
{
	lightrec_free_block_cache(state->block_cache);
	lightrec_free_block(state->wrapper);
	lightrec_free_block(state->addr_lookup_block);
	finish_jit();

	free(state);
}
