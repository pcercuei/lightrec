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

#include "emitter.h"
#include "regcache.h"

#include <lightning.h>

void emit_call_to_interpreter(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	/* TODO: Generate something... */
}

static uintptr_t __get_jump_address_cb(u32 pc)
{
	/* TODO: Recompile the block located at pc, and return its address */
	return lightrec_state.end_of_block;
}

static void lightrec_emit_end_of_block(jit_state_t *_jit,
		u8 reg_new_pc, u32 imm)
{
	jit_name(__func__);
	lightrec_free_regs();
	lightrec_storeback_all_regs(_jit);

	jit_note(__FILE__, __LINE__);

	/* FIXME: This may not work on all architectures */
	if (!reg_new_pc)
		jit_movi(JIT_RA0, imm);
	else
		jit_movr(JIT_RA0, reg_new_pc);

	jit_calli(&__get_jump_address_cb);
	jit_retval(JIT_R0);
	jit_jmpr(JIT_R0);
}

void rec_special_JR(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	u8 rs = lightrec_alloc_reg_in(_jit, op.r.rs);
	lightrec_emit_end_of_block(_jit, rs, 0);
}

void rec_J(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	lightrec_emit_end_of_block(_jit, 0, op.j.imm);
}

void rec_special_ADDU(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	u8 rs, rt, rd;

	jit_name(__func__);

	rs = lightrec_alloc_reg_in(_jit, op.r.rs);
	rt = lightrec_alloc_reg_in(_jit, op.r.rt);
	rd = lightrec_alloc_reg_out(_jit, op.r.rd);

	jit_note(__FILE__, __LINE__);
	jit_addr(rd, rs, rt);

	lightrec_free_regs();
}
