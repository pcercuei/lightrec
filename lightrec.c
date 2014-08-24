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
#include "recompiler.h"
#include "regcache.h"

#include <lightning.h>
#include <string.h>

/* When exiting, the recompiled code will jump to that address */
uintptr_t end_of_block;

static struct block *wrapper;

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
	for (i = 0; i < JIT_V_NUM; i++)
		jit_movr(JIT_V(i), JIT_V(i));

	/* Call the block's code */
	jit_jmpr(JIT_RA0);

	jit_note(__FILE__, __LINE__);

	/* The block will not return, but jump right here */
	addr = jit_indirect();
	jit_epilog();

	block->_jit = _jit;
	block->function = jit_emit();
	block->opcode_list = NULL;

	end_of_block = (uintptr_t) jit_address(addr);

	jit_clear_state();
	jit_disassemble();

	/* We're done! */
	return block;

err_free_block:
	free(block);
err_no_mem:
	ERROR("Unable to compile wrapper: Out of memory\n");
	return NULL;
}

struct block * lightrec_recompile_block(const u32 *code)
{
	struct opcode_list *elm, *list;
	struct block *block;
	jit_state_t *_jit;

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

	for (elm = list; elm; elm = SLIST_NEXT(elm, next))
		lightrec_rec_opcode(_jit, elm->opcode);

	block->_jit = _jit;
	block->function = jit_emit();
	block->opcode_list = list;
	return block;

err_free_list:
	lightrec_free_opcode_list(list);
err_free_block:
	free(block);
err_no_mem:
	ERROR("Unable to recompile block: Out of memory\n");
	return NULL;
}

void lightrec_execute_block(struct block *block)
{
	void (*func)(void *) = (void (*)(void *)) wrapper->function;
	func((void *) block->function);
}

void lightrec_init(char *argv0)
{
	init_jit(argv0);
	wrapper = generate_wrapper_block();
}
