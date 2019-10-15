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
#include "config.h"
#include "debug.h"
#include "disassembler.h"
#include "emitter.h"
#include "interpreter.h"
#include "lightrec.h"
#include "memmanager.h"
#include "recompiler.h"
#include "regcache.h"
#include "optimizer.h"

#include <errno.h>
#include <lightning.h>
#include <limits.h>
#if ENABLE_THREADED_COMPILER
#include <stdatomic.h>
#endif
#include <stdbool.h>
#include <stddef.h>
#include <string.h>

#define GENMASK(h, l) \
	(((uintptr_t)-1 << (l)) & ((uintptr_t)-1 >> (__WORDSIZE - 1 - (h))))

static struct block * lightrec_precompile_block(struct lightrec_state *state,
						u32 pc);

static void __segfault_cb(struct lightrec_state *state, u32 addr)
{
	lightrec_set_exit_flags(state, LIGHTREC_EXIT_SEGFAULT);
	pr_err("Segmentation fault in recompiled code: invalid "
	       "load/store at address 0x%08x\n", addr);
}

static u32 lightrec_rw_ops(struct lightrec_state *state, union code op,
		const struct lightrec_mem_map_ops *ops, u32 addr, u32 data)
{
	switch (op.i.op) {
	case OP_SB:
		ops->sb(state, addr, (u8) data);
		return 0;
	case OP_SH:
		ops->sh(state, addr, (u16) data);
		return 0;
	case OP_SWL:
	case OP_SWR:
	case OP_SW:
		ops->sw(state, addr, data);
		return 0;
	case OP_LB:
		return (s32) (s8) ops->lb(state, addr);
	case OP_LBU:
		return ops->lb(state, addr);
	case OP_LH:
		return (s32) (s16) ops->lh(state, addr);
	case OP_LHU:
		return ops->lh(state, addr);
	case OP_LW:
	default:
		return ops->lw(state, addr);
	}
}

static void lightrec_invalidate_map(struct lightrec_state *state,
		const struct lightrec_mem_map *map, u32 addr)
{
	if (map == &state->maps[PSX_MAP_KERNEL_USER_RAM])
		state->code_lut[addr >> 2] = NULL;
}

static const struct lightrec_mem_map *
lightrec_get_map(struct lightrec_state *state, u32 kaddr)
{
	unsigned int i;

	for (i = 0; i < state->nb_maps; i++) {
		const struct lightrec_mem_map *map = &state->maps[i];

		if (kaddr >= map->pc && kaddr < map->pc + map->length)
			return map;
	}

	return NULL;
}

u32 lightrec_rw(struct lightrec_state *state, union code op,
		u32 addr, u32 data, u16 *flags)
{
	const struct lightrec_mem_map *map;
	u32 shift, mem_data, mask, pc;
	uintptr_t new_addr;
	u32 kaddr;

	addr += (s16) op.i.imm;
	kaddr = kunseg(addr);

	map = lightrec_get_map(state, kaddr);
	if (!map) {
		__segfault_cb(state, addr);
		return 0;
	}

	pc = map->pc;

	if (unlikely(map->ops))
		return lightrec_rw_ops(state, op, map->ops, addr, data);

	while (map->mirror_of)
		map = map->mirror_of;

	if (flags)
		*flags |= LIGHTREC_DIRECT_IO;

	kaddr -= pc;
	new_addr = (uintptr_t) map->address + kaddr;

	switch (op.i.op) {
	case OP_SB:
		*(u8 *) new_addr = (u8) data;
		lightrec_invalidate_map(state, map, kaddr);
		return 0;
	case OP_SH:
		*(u16 *) new_addr = HTOLE16((u16) data);
		lightrec_invalidate_map(state, map, kaddr);
		return 0;
	case OP_SWL:
		shift = kaddr & 3;
		mem_data = LE32TOH(*(u32 *)(new_addr & ~3));
		mask = GENMASK(31, (shift + 1) * 8);

		*(u32 *)(new_addr & ~3) = HTOLE32((data >> ((3 - shift) * 8))
						  | (mem_data & mask));
		lightrec_invalidate_map(state, map, kaddr & ~0x3);
		return 0;
	case OP_SWR:
		shift = kaddr & 3;
		mem_data = LE32TOH(*(u32 *)(new_addr & ~3));
		mask = (1 << (shift * 8)) - 1;

		*(u32 *)(new_addr & ~3) = HTOLE32((data << (shift * 8))
						  | (mem_data & mask));
		lightrec_invalidate_map(state, map, kaddr & ~0x3);
		return 0;
	case OP_SW:
		*(u32 *) new_addr = HTOLE32(data);
		lightrec_invalidate_map(state, map, kaddr);
		return 0;
	case OP_SWC2:
		*(u32 *) new_addr = HTOLE32(state->ops.cop2_ops.mfc(state,
								    op.i.rt));
		lightrec_invalidate_map(state, map, kaddr);
		return 0;
	case OP_LB:
		return (s32) *(s8 *) new_addr;
	case OP_LBU:
		return *(u8 *) new_addr;
	case OP_LH:
		return (s32)(s16) LE16TOH(*(u16 *) new_addr);
	case OP_LHU:
		return LE16TOH(*(u16 *) new_addr);
	case OP_LWL:
		shift = kaddr & 3;
		mem_data = LE32TOH(*(u32 *)(new_addr & ~3));
		mask = (1 << (24 - shift * 8)) - 1;

		return (data & mask) | (mem_data << (24 - shift * 8));
	case OP_LWR:
		shift = kaddr & 3;
		mem_data = LE32TOH(*(u32 *)(new_addr & ~3));
		mask = GENMASK(31, 32 - shift * 8);

		return (data & mask) | (mem_data >> (shift * 8));
	case OP_LWC2:
		state->ops.cop2_ops.mtc(state, op.i.rt,
					LE32TOH(*(u32 *) new_addr));
		return 0;
	case OP_LW:
	default:
		return LE32TOH(*(u32 *) new_addr);
	}
}

static void lightrec_rw_cb(struct lightrec_state *state, union code op)
{
	u32 ret;

	ret = lightrec_rw(state, op,
			  state->native_reg_cache[op.i.rs],
			  state->native_reg_cache[op.i.rt], NULL);

	switch (op.i.op) {
	case OP_LB:
	case OP_LBU:
	case OP_LH:
	case OP_LHU:
	case OP_LWL:
	case OP_LWR:
	case OP_LW:
		if (op.i.rt)
			state->native_reg_cache[op.i.rt] = ret;
	default: /* fall-through */
		break;
	}
}

u32 lightrec_mfc(struct lightrec_state *state, union code op)
{
	bool is_cfc = (op.i.op == OP_CP0 && op.r.rs == OP_CP0_CFC0) ||
		      (op.i.op == OP_CP2 && op.r.rs == OP_CP2_BASIC_CFC2);
	u32 (*func)(struct lightrec_state *, u8);
	const struct lightrec_cop_ops *ops;

	if (op.i.op == OP_CP0)
		ops = &state->ops.cop0_ops;
	else
		ops = &state->ops.cop2_ops;

	if (is_cfc)
		func = ops->cfc;
	else
		func = ops->mfc;

	return (*func)(state, op.r.rd);
}

static void lightrec_mfc_cb(struct lightrec_state *state, union code op)
{
	u32 rt = lightrec_mfc(state, op);

	if (op.r.rt)
		state->native_reg_cache[op.r.rt] = rt;
}

void lightrec_mtc(struct lightrec_state *state, union code op, u32 data)
{
	bool is_ctc = (op.i.op == OP_CP0 && op.r.rs == OP_CP0_CTC0) ||
		      (op.i.op == OP_CP2 && op.r.rs == OP_CP2_BASIC_CTC2);
	void (*func)(struct lightrec_state *, u8, u32);
	const struct lightrec_cop_ops *ops;

	if (op.i.op == OP_CP0)
		ops = &state->ops.cop0_ops;
	else
		ops = &state->ops.cop2_ops;

	if (is_ctc)
		func = ops->ctc;
	else
		func = ops->mtc;

	(*func)(state, op.r.rd, data);
}

static void lightrec_mtc_cb(struct lightrec_state *state, union code op)
{
	lightrec_mtc(state, op, state->native_reg_cache[op.r.rt]);
}

static void lightrec_rfe_cb(struct lightrec_state *state, union code op)
{
	u32 status;

	/* Read CP0 Status register (r12) */
	status = state->ops.cop0_ops.mfc(state, 12);

	/* Switch the bits */
	status = ((status & 0x3c) >> 2) | (status & ~0xf);

	/* Write it back */
	state->ops.cop0_ops.ctc(state, 12, status);
}

static void lightrec_cp_cb(struct lightrec_state *state, union code op)
{
	void (*func)(struct lightrec_state *, u32);

	if ((op.opcode >> 25) & 1)
		func = state->ops.cop2_ops.op;
	else
		func = state->ops.cop0_ops.op;

	(*func)(state, op.opcode);
}

static void lightrec_syscall_cb(struct lightrec_state *state, union code op)
{
	lightrec_set_exit_flags(state, LIGHTREC_EXIT_SYSCALL);
}

static void lightrec_break_cb(struct lightrec_state *state, union code op)
{
	lightrec_set_exit_flags(state, LIGHTREC_EXIT_BREAK);
}

struct block * lightrec_get_block(struct lightrec_state *state, u32 pc)
{
	struct block *block = lightrec_find_block(state->block_cache, pc);

	if (block && lightrec_block_is_outdated(block)) {
		pr_debug("Block at PC 0x%08x is outdated!\n", block->pc);

		/* Make sure the recompiler isn't processing the block we'll
		 * destroy */
		if (ENABLE_THREADED_COMPILER)
			lightrec_recompiler_remove(state->rec, block);

		lightrec_unregister_block(state->block_cache, block);
		lightrec_free_block(block);
		block = NULL;
	}

	if (!block) {
		block = lightrec_precompile_block(state, pc);
		if (!block) {
			pr_err("Unable to recompile block at PC 0x%x\n", pc);
			lightrec_set_exit_flags(state, LIGHTREC_EXIT_SEGFAULT);
			return NULL;
		}

		lightrec_register_block(state->block_cache, block);
	}

	return block;
}

static void * get_next_block_func(struct lightrec_state *state, u32 pc)
{
	for (;;) {
		struct block *block = lightrec_get_block(state, pc);
		void *func;

		if (unlikely(!block))
			return NULL;

		if (ENABLE_FIRST_PASS && ENABLE_THREADED_COMPILER)
			func = lightrec_recompiler_run_first_pass(block, &pc);
		else
			func = block->function;

		if (likely(func))
			return func;

		/* Block wasn't compiled yet - run the interpreter */
		if (ENABLE_FIRST_PASS && !ENABLE_THREADED_COMPILER)
			pc = lightrec_emulate_block(block);

		if (likely(!(block->flags & BLOCK_NEVER_COMPILE))) {
			/* Then compile it using the profiled data */
			if (ENABLE_THREADED_COMPILER)
				lightrec_recompiler_add(state->rec, block);
			else
				lightrec_compile_block(block);
		}

		if (state->exit_flags != LIGHTREC_EXIT_NORMAL ||
		    state->current_cycle >= state->target_cycle) {
			state->next_pc = pc;
			return NULL;
		}
	}
}

static s32 c_function_wrapper(struct lightrec_state *state, s32 cycles_delta,
			      void (*f)(struct lightrec_state *, union code),
			      union code op)
{
	state->current_cycle = state->target_cycle - cycles_delta;

	(*f)(state, op);

	return state->target_cycle - state->current_cycle;
}

static struct block * generate_wrapper(struct lightrec_state *state,
				       void (*f)(struct lightrec_state *,
						 union code))
{
	struct block *block;
	jit_state_t *_jit;
	unsigned int i;
	int stack_ptr;
	jit_word_t code_size;
	jit_node_t *to_tramp, *to_fn_epilog;

	block = lightrec_malloc(MEM_FOR_IR, sizeof(*block));
	if (!block)
		goto err_no_mem;

	_jit = jit_new_state();
	if (!_jit)
		goto err_free_block;

	jit_name("RW wrapper");
	jit_note(__FILE__, __LINE__);

	/* Wrapper entry point */
	jit_prolog();

	stack_ptr = jit_allocai(sizeof(uintptr_t) * NUM_TEMPS);

	for (i = 0; i < NUM_TEMPS; i++)
		jit_stxi(stack_ptr + i * sizeof(uintptr_t), JIT_FP, JIT_R(i));

	/* Jump to the trampoline */
	to_tramp = jit_jmpi();

	/* The trampoline will jump back here */
	to_fn_epilog = jit_label();

	for (i = 0; i < NUM_TEMPS; i++)
		jit_ldxi(JIT_R(i), JIT_FP, stack_ptr + i * sizeof(uintptr_t));

	jit_ret();
	jit_epilog();

	/* Trampoline entry point.
	 * The sole purpose of the trampoline is to cheese Lightning not to
	 * save/restore the callee-saved register LIGHTREC_REG_CYCLE, since we
	 * do want to return to the caller with this register modified. */
	jit_prolog();
	jit_tramp(256);
	jit_patch(to_tramp);

	jit_prepare();
	jit_pushargr(LIGHTREC_REG_STATE);
	jit_pushargr(LIGHTREC_REG_CYCLE);
	jit_pushargi((uintptr_t)f);
	jit_pushargr(JIT_R0);

	jit_finishi(c_function_wrapper);

#if __WORDSIZE == 64
	jit_retval_i(LIGHTREC_REG_CYCLE);
#else
	jit_retval(LIGHTREC_REG_CYCLE);
#endif

	jit_patch_at(jit_jmpi(), to_fn_epilog);
	jit_epilog();

	block->state = state;
	block->_jit = _jit;
	block->function = jit_emit();
	block->opcode_list = NULL;
	block->flags = 0;
	block->nb_ops = 0;

	jit_get_code(&code_size);
	lightrec_register(MEM_FOR_CODE, code_size);

	block->code_size = code_size;

	if (ENABLE_DISASSEMBLER) {
		pr_debug("Wrapper block:\n");
		jit_disassemble();
	}

	jit_clear_state();
	return block;

err_free_block:
	lightrec_free(MEM_FOR_IR, sizeof(*block), block);
err_no_mem:
	pr_err("Unable to compile wrapper: Out of memory\n");
	return NULL;
}

static struct block * generate_wrapper_block(struct lightrec_state *state)
{
	struct block *block;
	jit_state_t *_jit;
	jit_node_t *to_end, *to_end2, *to_c, *loop, *addr, *addr2;
	unsigned int i;
	u32 offset, ram_len;
	jit_word_t code_size;

	block = lightrec_malloc(MEM_FOR_IR, sizeof(*block));
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
#if __WORDSIZE == 64
	jit_getarg_i(LIGHTREC_REG_CYCLE, jit_arg());
#else
	jit_getarg(LIGHTREC_REG_CYCLE, jit_arg());
#endif

	/* Force all callee-saved registers to be pushed on the stack */
	for (i = 0; i < NUM_REGS; i++)
		jit_movr(JIT_V(i), JIT_V(i));

	/* Pass lightrec_state structure to blocks, using the last callee-saved
	 * register that Lightning provides */
	jit_movi(LIGHTREC_REG_STATE, (intptr_t) state);

	loop = jit_label();

	/* Call the block's code */
	jit_jmpr(JIT_R0);

	/* The block will jump here, with the number of cycles remaining in
	 * LIGHTREC_REG_CYCLE */
	addr2 = jit_indirect();

	/* Jump to end if state->target_cycle < state->current_cycle */
	to_end = jit_blei(LIGHTREC_REG_CYCLE, 0);

	/* Convert next PC to KUNSEG and avoid mirrors */
	ram_len = state->maps[PSX_MAP_KERNEL_USER_RAM].length;
	jit_andi(JIT_R0, JIT_V0, 0x10000000 | (ram_len - 1));
	to_c = jit_bgei(JIT_R0, ram_len);

	/* Fast path: code is running from RAM, use the code LUT */
#if __WORDSIZE == 64
	jit_lshi(JIT_R0, JIT_R0, 1);
#endif
	jit_addr(JIT_R0, JIT_R0, LIGHTREC_REG_STATE);
	jit_ldxi(JIT_R0, JIT_R0, offsetof(struct lightrec_state, code_lut));

	/* If we get non-NULL, loop */
	jit_patch_at(jit_bnei(JIT_R0, 0), loop);

	/* Slow path: call C function get_next_block_func() */
	jit_patch(to_c);

	if (ENABLE_FIRST_PASS) {
		/* We may call the interpreter - update state->current_cycle */
		jit_ldxi_i(JIT_R2, LIGHTREC_REG_STATE,
			   offsetof(struct lightrec_state, target_cycle));
		jit_subr(JIT_R1, JIT_R2, LIGHTREC_REG_CYCLE);
		jit_stxi_i(offsetof(struct lightrec_state, current_cycle),
			   LIGHTREC_REG_STATE, JIT_R1);
	}

	/* The code LUT will be set to this address when the block at the target
	 * PC has been preprocessed but not yet compiled by the threaded
	 * recompiler */
	addr = jit_indirect();

	/* Get the next block */
	jit_prepare();
	jit_pushargr(LIGHTREC_REG_STATE);
	jit_pushargr(JIT_V0);
	jit_finishi(&get_next_block_func);
	jit_retval(JIT_R0);

	if (ENABLE_FIRST_PASS) {
		/* The interpreter may have updated state->current_cycle and
		 * state->target_cycle - recalc the delta */
		jit_ldxi_i(JIT_R1, LIGHTREC_REG_STATE,
			   offsetof(struct lightrec_state, current_cycle));
		jit_ldxi_i(JIT_R2, LIGHTREC_REG_STATE,
			   offsetof(struct lightrec_state, target_cycle));
		jit_subr(LIGHTREC_REG_CYCLE, JIT_R2, JIT_R1);
	}

	/* If we get non-NULL, loop */
	jit_patch_at(jit_bnei(JIT_R0, 0), loop);

	to_end2 = jit_jmpi();

	/* When exiting, the recompiled code will jump to that address */
	jit_note(__FILE__, __LINE__);
	jit_patch(to_end);

	/* Store back the next_pc to the lightrec_state structure */
	offset = offsetof(struct lightrec_state, next_pc);
	jit_stxi_i(offset, LIGHTREC_REG_STATE, JIT_V0);

	jit_patch(to_end2);

	jit_retr(LIGHTREC_REG_CYCLE);
	jit_epilog();

	block->state = state;
	block->_jit = _jit;
	block->function = jit_emit();
	block->opcode_list = NULL;
	block->flags = 0;
	block->nb_ops = 0;

	jit_get_code(&code_size);
	lightrec_register(MEM_FOR_CODE, code_size);

	block->code_size = code_size;

	state->eob_wrapper_func = jit_address(addr2);
	state->get_next_block = jit_address(addr);

	if (ENABLE_DISASSEMBLER) {
		pr_debug("Main wrapper block:\n");
		jit_disassemble();
	}

	/* We're done! */
	jit_clear_state();
	return block;

err_free_block:
	lightrec_free(MEM_FOR_IR, sizeof(*block), block);
err_no_mem:
	pr_err("Unable to compile wrapper: Out of memory\n");
	return NULL;
}

union code lightrec_read_opcode(struct lightrec_state *state, u32 pc)
{
	u32 addr, kunseg_pc = kunseg(pc);
	const u32 *code;
	const struct lightrec_mem_map *map = lightrec_get_map(state, kunseg_pc);

	addr = kunseg_pc - map->pc;

	while (map->mirror_of)
		map = map->mirror_of;

	code = map->address + addr;

	return (union code) *code;
}

static struct block * lightrec_precompile_block(struct lightrec_state *state,
						u32 pc)
{
	struct opcode *list;
	struct block *block;
	const u32 *code;
	u32 addr, kunseg_pc = kunseg(pc);
	const struct lightrec_mem_map *map = lightrec_get_map(state, kunseg_pc);
	unsigned int length;

	if (!map)
		return NULL;

	addr = kunseg_pc - map->pc;

	while (map->mirror_of)
		map = map->mirror_of;

	code = map->address + addr;

	block = lightrec_malloc(MEM_FOR_IR, sizeof(*block));
	if (!block) {
		pr_err("Unable to recompile block: Out of memory\n");
		return NULL;
	}

	list = lightrec_disassemble(code, &length);
	if (!list) {
		lightrec_free(MEM_FOR_IR, sizeof(*block), block);
		return NULL;
	}

	block->pc = pc;
	block->kunseg_pc = map->pc + addr;
	block->state = state;
	block->_jit = NULL;
	block->function = NULL;
	block->opcode_list = list;
	block->map = map;
	block->next = NULL;
	block->flags = 0;
	block->code_size = 0;
#if ENABLE_THREADED_COMPILER
	block->op_list_freed = (atomic_flag)ATOMIC_FLAG_INIT;
#endif
	block->nb_ops = length / sizeof(u32);

	lightrec_optimize(block);

	lightrec_register(MEM_FOR_MIPS_CODE, length);

	if (ENABLE_DISASSEMBLER) {
		pr_debug("Disassembled block at PC: 0x%x\n", block->pc);
		lightrec_print_disassembly(block, code, length);
	}

	return block;
}

int lightrec_compile_block(struct block *block)
{
	struct lightrec_state *state = block->state;
	bool op_list_freed = false;
	struct opcode *elm;
	jit_state_t *_jit;
	bool skip_next = false;
	jit_word_t code_size;
	unsigned int i;
	u32 next_pc;
	int ret;

	_jit = jit_new_state();
	if (!_jit)
		return -ENOMEM;

	block->_jit = _jit;

	lightrec_regcache_reset(state->reg_cache);
	state->cycles = 0;
	state->nb_branches = 0;

	jit_prolog();
	jit_tramp(256);

	for (elm = block->opcode_list; elm; elm = elm->next) {
		state->cycles += lightrec_cycles_of_opcode(elm->c);

		if (skip_next) {
			skip_next = false;
		} else if (elm->opcode) {
			next_pc = block->pc + elm->offset * sizeof(u32);
			lightrec_rec_opcode(block, elm, next_pc);
			skip_next = has_delay_slot(elm->c) &&
				!(elm->flags & LIGHTREC_NO_DS);
		}
	}

	for (i = 0; i < state->nb_branches; i++)
		jit_patch(state->branches[i]);

	jit_ldxi(JIT_R0, LIGHTREC_REG_STATE,
		 offsetof(struct lightrec_state, eob_wrapper_func));

	jit_jmpr(JIT_R0);

	jit_ret();
	jit_epilog();

	block->function = jit_emit();

	/* Add compiled function to the LUT */
	if (block->map == &block->state->maps[PSX_MAP_KERNEL_USER_RAM])
		block->state->code_lut[block->kunseg_pc >> 2] = block->function;

	jit_get_code(&code_size);
	lightrec_register(MEM_FOR_CODE, code_size);

	block->code_size = code_size;

	if (ENABLE_DISASSEMBLER) {
		pr_debug("Compiling block at PC: 0x%x\n", block->pc);
		jit_disassemble();
	}

	jit_clear_state();

#if ENABLE_THREADED_COMPILER
	op_list_freed = atomic_flag_test_and_set(&block->op_list_freed);
#endif
	if (!op_list_freed) {
		lightrec_free_opcode_list(block->opcode_list);
		block->opcode_list = NULL;
	}

	return 0;
}

u32 lightrec_execute(struct lightrec_state *state, u32 pc, u32 target_cycle)
{
	s32 (*func)(void *, s32) = (void *)state->wrapper->function;
	void *block_trace;
	s32 cycles_delta;

	state->exit_flags = LIGHTREC_EXIT_NORMAL;

	/* Handle the cycle counter overflowing */
	if (unlikely(target_cycle < state->current_cycle))
		target_cycle = UINT_MAX;

	state->target_cycle = target_cycle;

	block_trace = get_next_block_func(state, pc);
	if (block_trace) {
		cycles_delta = state->target_cycle - state->current_cycle;

		cycles_delta = (*func)(block_trace, cycles_delta);

		state->current_cycle = state->target_cycle - cycles_delta;
	}

	return state->next_pc;
}

u32 lightrec_execute_one(struct lightrec_state *state, u32 pc)
{
	return lightrec_execute(state, pc, state->current_cycle);
}

u32 lightrec_run_interpreter(struct lightrec_state *state, u32 pc)
{
	struct block *block = lightrec_get_block(state, pc);
	if (!block)
		return 0;

	state->exit_flags = LIGHTREC_EXIT_NORMAL;

	return lightrec_emulate_block(block);
}

void lightrec_free_block(struct block *block)
{
	lightrec_unregister(MEM_FOR_MIPS_CODE, block->nb_ops * sizeof(u32));
	if (block->opcode_list)
		lightrec_free_opcode_list(block->opcode_list);
	if (block->_jit)
		_jit_destroy_state(block->_jit);
	lightrec_unregister(MEM_FOR_CODE, block->code_size);
	lightrec_free(MEM_FOR_IR, sizeof(*block), block);
}

struct lightrec_state * lightrec_init(char *argv0,
				      const struct lightrec_mem_map *map,
				      size_t nb,
				      const struct lightrec_ops *ops)
{
	struct lightrec_state *state;
	unsigned int lut_size;

	/* Sanity-check ops */
	if (!ops ||
	    !ops->cop0_ops.mfc || !ops->cop0_ops.cfc || !ops->cop0_ops.mtc ||
	    !ops->cop0_ops.ctc || !ops->cop0_ops.op ||
	    !ops->cop2_ops.mfc || !ops->cop2_ops.cfc || !ops->cop2_ops.mtc ||
	    !ops->cop2_ops.ctc || !ops->cop2_ops.op) {
		pr_err("Missing callbacks in lightrec_ops structure\n");
		return NULL;
	}

	init_jit(argv0);

	lut_size = map[PSX_MAP_KERNEL_USER_RAM].length >> 2;

	state = lightrec_calloc(MEM_FOR_LIGHTREC, sizeof(*state) +
				sizeof(*state->code_lut) * lut_size);
	if (!state)
		goto err_finish_jit;

	state->lut_size = lut_size;

	state->block_cache = lightrec_blockcache_init();
	if (!state->block_cache)
		goto err_free_state;

	state->reg_cache = lightrec_regcache_init();
	if (!state->reg_cache)
		goto err_free_block_cache;

	if (ENABLE_THREADED_COMPILER) {
		state->rec = lightrec_recompiler_init();
		if (!state->rec)
			goto err_free_reg_cache;
	}

	state->nb_maps = nb;
	state->maps = map;

	memcpy(&state->ops, ops, sizeof(*ops));

	state->wrapper = generate_wrapper_block(state);
	if (!state->wrapper)
		goto err_free_recompiler;

	state->rw_wrapper = generate_wrapper(state, lightrec_rw_cb);
	if (!state->rw_wrapper)
		goto err_free_wrapper;

	state->mfc_wrapper = generate_wrapper(state, lightrec_mfc_cb);
	if (!state->mfc_wrapper)
		goto err_free_rw_wrapper;

	state->mtc_wrapper = generate_wrapper(state, lightrec_mtc_cb);
	if (!state->mtc_wrapper)
		goto err_free_mfc_wrapper;

	state->rfe_wrapper = generate_wrapper(state, lightrec_rfe_cb);
	if (!state->rfe_wrapper)
		goto err_free_mtc_wrapper;

	state->cp_wrapper = generate_wrapper(state, lightrec_cp_cb);
	if (!state->cp_wrapper)
		goto err_free_rfe_wrapper;

	state->syscall_wrapper = generate_wrapper(state, lightrec_syscall_cb);
	if (!state->syscall_wrapper)
		goto err_free_cp_wrapper;

	state->break_wrapper = generate_wrapper(state, lightrec_break_cb);
	if (!state->break_wrapper)
		goto err_free_syscall_wrapper;

	state->rw_func = state->rw_wrapper->function;
	state->mfc_func = state->mfc_wrapper->function;
	state->mtc_func = state->mtc_wrapper->function;
	state->rfe_func = state->rfe_wrapper->function;
	state->cp_func = state->cp_wrapper->function;
	state->syscall_func = state->syscall_wrapper->function;
	state->break_func = state->break_wrapper->function;

	map = &state->maps[PSX_MAP_BIOS];
	state->offset_bios = (uintptr_t)map->address - map->pc;

	map = &state->maps[PSX_MAP_SCRATCH_PAD];
	state->offset_scratch = (uintptr_t)map->address - map->pc;

	map = &state->maps[PSX_MAP_KERNEL_USER_RAM];
	state->offset_ram = (uintptr_t)map->address - map->pc;

	if (state->maps[PSX_MAP_MIRROR1].address == map->address + 0x200000 &&
	    state->maps[PSX_MAP_MIRROR2].address == map->address + 0x400000 &&
	    state->maps[PSX_MAP_MIRROR3].address == map->address + 0x600000)
		state->mirrors_mapped = true;

	return state;

err_free_syscall_wrapper:
	lightrec_free_block(state->syscall_wrapper);
err_free_cp_wrapper:
	lightrec_free_block(state->cp_wrapper);
err_free_rfe_wrapper:
	lightrec_free_block(state->rfe_wrapper);
err_free_mtc_wrapper:
	lightrec_free_block(state->mtc_wrapper);
err_free_mfc_wrapper:
	lightrec_free_block(state->mfc_wrapper);
err_free_rw_wrapper:
	lightrec_free_block(state->rw_wrapper);
err_free_wrapper:
	lightrec_free_block(state->wrapper);
err_free_recompiler:
	if (ENABLE_THREADED_COMPILER)
		lightrec_free_recompiler(state->rec);
err_free_reg_cache:
	lightrec_free_regcache(state->reg_cache);
err_free_block_cache:
	lightrec_free_block_cache(state->block_cache);
err_free_state:
	lightrec_free(MEM_FOR_LIGHTREC, sizeof(*state) +
		      sizeof(*state->code_lut) * state->lut_size, state);
err_finish_jit:
	finish_jit();
	return NULL;
}

void lightrec_destroy(struct lightrec_state *state)
{
	if (ENABLE_THREADED_COMPILER)
		lightrec_free_recompiler(state->rec);

	lightrec_free_regcache(state->reg_cache);
	lightrec_free_block_cache(state->block_cache);
	lightrec_free_block(state->wrapper);
	lightrec_free_block(state->rw_wrapper);
	lightrec_free_block(state->mfc_wrapper);
	lightrec_free_block(state->mtc_wrapper);
	lightrec_free_block(state->rfe_wrapper);
	lightrec_free_block(state->cp_wrapper);
	lightrec_free_block(state->syscall_wrapper);
	lightrec_free_block(state->break_wrapper);
	finish_jit();

	lightrec_free(MEM_FOR_LIGHTREC, sizeof(*state) +
		      sizeof(*state->code_lut) * state->lut_size, state);
}

void lightrec_invalidate(struct lightrec_state *state, u32 addr, u32 len)
{
	u32 kaddr = kunseg(addr & ~0x3);
	const struct lightrec_mem_map *map = lightrec_get_map(state, kaddr);

	if (map) {
		while (map->mirror_of)
			map = map->mirror_of;

		if (map != &state->maps[PSX_MAP_KERNEL_USER_RAM])
			return;

		/* Handle mirrors */
		kaddr &= (state->maps[PSX_MAP_KERNEL_USER_RAM].length - 1);

		for (; len > 4; len -= 4, kaddr += 4)
			lightrec_invalidate_map(state, map, kaddr);

		lightrec_invalidate_map(state, map, kaddr);
	}
}

void lightrec_invalidate_all(struct lightrec_state *state)
{
	memset(state->code_lut, 0,
	       state->maps[PSX_MAP_KERNEL_USER_RAM].length >> 2);
}

void lightrec_set_exit_flags(struct lightrec_state *state, u32 flags)
{
	if (flags != LIGHTREC_EXIT_NORMAL) {
		state->exit_flags |= flags;
		state->target_cycle = state->current_cycle;
	}
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

u32 lightrec_current_cycle_count(const struct lightrec_state *state)
{
	return state->current_cycle;
}

void lightrec_reset_cycle_count(struct lightrec_state *state, u32 cycles)
{
	state->current_cycle = cycles;

	if (state->target_cycle < cycles)
		state->target_cycle = cycles;
}

void lightrec_set_target_cycle_count(struct lightrec_state *state, u32 cycles)
{
	if (state->exit_flags == LIGHTREC_EXIT_NORMAL) {
		if (cycles < state->current_cycle)
			cycles = state->current_cycle;

		state->target_cycle = cycles;
	}
}
