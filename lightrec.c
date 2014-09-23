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

static struct block * generate_address_lookup_block(unsigned int nb_maps)
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
	jit_getarg(JIT_RA0, jit_arg());

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
	jit_ldxi_i(JIT_V2, JIT_V0, offsetof(struct lightrec_mem_map, pc));
	addr = jit_bltr_u(JIT_RA0, JIT_V2);

	/* Test if addr < curr_map->pc + curr_map->length */
	jit_ldxi_i(JIT_V1, JIT_V0, offsetof(struct lightrec_mem_map, length));
	jit_addr(JIT_V1, JIT_V2, JIT_V1);
	addr2 = jit_bger_u(JIT_RA0, JIT_V1);

	/* Found: calculate address and jump to end */
	jit_ldxi(JIT_V1, JIT_V0, offsetof(struct lightrec_mem_map, address));
	jit_subr(JIT_V2, JIT_RA0, JIT_V2);
	jit_addr(JIT_V2, JIT_V2, JIT_V1);
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

	/* Reset LIGHTREC_REG_STATE to its correct value */
	jit_subi(LIGHTREC_REG_STATE, LIGHTREC_REG_STATE,
			offsetof(struct lightrec_state, mem_map));

	/* And return the address to the caller */
	jit_movr(JIT_RA0, JIT_V2);
	jit_epilog();

	block->_jit = _jit;
	block->function = jit_emit();
	block->opcode_list = NULL;
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
	jit_getarg(JIT_RA0, jit_arg());

	/* Force all callee-saved registers to be pushed on the stack */
	for (i = 0; i < NUM_REGS; i++)
		jit_movr(JIT_V(i), JIT_V(i));

	/* Pass lightrec_state structure to blocks, using the last callee-saved
	 * register that Lightning provides */
	jit_movi(LIGHTREC_REG_STATE, state);

	/* Call the block's code */
	jit_jmpr(JIT_RA0);

	jit_note(__FILE__, __LINE__);

	/* The block will not return, but jump right here */
	addr = jit_indirect();
	jit_epilog();

	block->_jit = _jit;
	block->function = jit_emit();
	block->opcode_list = NULL;

	/* When exiting, the recompiled code will jump to that address */
	state->end_of_block = (uintptr_t) jit_address(addr);

	/* We're done! */
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
	block->_jit = _jit;
	block->opcode_list = list;

	for (elm = list; elm; elm = SLIST_NEXT(elm, next), pc += 4) {
		int ret;

		if (skip_next) {
			skip_next = false;
			continue;
		}

		/* Don't recompile NOPs */
		if (!elm->opcode.opcode)
			continue;

		ret = lightrec_rec_opcode(_jit, elm->opcode, block, pc);
		skip_next = ret == SKIP_DELAY_SLOT;
	}

	block->function = jit_emit();

#if (LOG_LEVEL >= DEBUG_L)
	DEBUG("Recompiling block at PC: 0x%x\n", block->pc);
	jit_disassemble();
#endif
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
		struct lightrec_mem_map *map, unsigned int nb)
{
	struct lightrec_state *state;

	init_jit(argv0);

	state = calloc(1, sizeof(*state) + nb * sizeof(*map));
	state->block_cache = lightrec_blockcache_init();

	state->nb_maps = nb;
	memcpy(state->mem_map, map, nb * sizeof(*map));

	state->wrapper = generate_wrapper_block(state);

	state->addr_lookup_block = generate_address_lookup_block(nb);
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
