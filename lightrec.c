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
#include <string.h>

struct lightrec_state *lightrec_state;

static struct block *wrapper;

static const u32 * find_code_address(u32 pc)
{
	struct lightrec_mem_map *map = lightrec_state->mem_map;
	unsigned int i;

	for (i = 0; i < lightrec_state->nb_maps; i++) {
		if (pc >= map[i].pc && pc < map[i].pc + map[i].length)
			return map[i].address + (pc - map[i].pc);
	}

	return NULL;
}

static struct block * generate_wrapper_block(void)
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
	jit_movi(LIGHTREC_REG_STATE, lightrec_state);

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
	lightrec_state->end_of_block = (uintptr_t) jit_address(addr);

	/* We're done! */
	return block;

err_free_block:
	free(block);
err_no_mem:
	ERROR("Unable to compile wrapper: Out of memory\n");
	return NULL;
}

struct block * lightrec_recompile_block(u32 pc)
{
	struct opcode_list *elm, *list;
	struct block *block;
	jit_state_t *_jit;
	bool skip_next = false;
	const u32 *code = find_code_address(pc);
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
	return block;

err_free_list:
	lightrec_free_opcode_list(list);
err_free_block:
	free(block);
err_no_mem:
	ERROR("Unable to recompile block: Out of memory\n");
	return NULL;
}

u32 lightrec_execute(u32 pc)
{
	void (*func)(void *) = (void (*)(void *)) wrapper->function;
	struct block *block = lightrec_find_block(pc);

	if (!block) {
		block = lightrec_recompile_block(pc);
		if (!block) {
			ERROR("Unable to recompile block at PC 0x%x\n", pc);
			return pc;
		}

		lightrec_register_block(block);
	}

	func((void *) block->function);
	return lightrec_state->next_pc;
}

void lightrec_free_block(struct block *block)
{
	lightrec_free_opcode_list(block->opcode_list);
	_jit_destroy_state(block->_jit);
	free(block);
}

void lightrec_init(char *argv0, struct lightrec_mem_map *map, unsigned int nb)
{
	init_jit(argv0);

	lightrec_state = calloc(1, sizeof(*lightrec_state)
			+ nb * sizeof(struct lightrec_mem_map));

	lightrec_state->nb_maps = nb;
	memcpy(lightrec_state->mem_map, map, nb * sizeof(*map));

	wrapper = generate_wrapper_block();
}

void lightrec_destroy(void)
{
	lightrec_free_block_cache();
	lightrec_free_block(wrapper);
	finish_jit();
}
