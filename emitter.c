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
#include "emitter.h"
#include "regcache.h"

#include <lightning.h>
#include <stddef.h>

void emit_call_to_interpreter(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	/* TODO: Generate something... */
}

static uintptr_t __get_jump_address_cb(u32 pc)
{
	struct block *new;

	if (lightrec_state.stop)
		return lightrec_state.end_of_block;

	new = lightrec_find_block(pc);
	if (!new) {
		new = lightrec_recompile_block(pc);
		if (!new)
			return lightrec_state.end_of_block;
		lightrec_register_block(new);
	}

	return (uintptr_t) new->function;
}

static void lightrec_emit_end_of_block(jit_state_t *_jit,
		u8 reg_new_pc, u32 imm, u32 link)
{
	lightrec_storeback_regs(_jit);

	jit_note(__FILE__, __LINE__);

	if (link) {
		/* Update the $ra register */
		s16 offset = offsetof(struct lightrec_state, reg_cache)
			+ (31 << 2);
		jit_movi(JIT_R0, link);
		jit_stxi_i(offset, LIGHTREC_REG_STATE, JIT_R0);
	}

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

	jit_name(__func__);
	lightrec_emit_end_of_block(_jit, rs, 0, 0);
}

void rec_special_JALR(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	u8 rs = lightrec_alloc_reg_in(_jit, op.r.rs);

	jit_name(__func__);
	lightrec_emit_end_of_block(_jit, rs, 0, pc + 8);
}

void rec_J(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	lightrec_emit_end_of_block(_jit, 0, op.j.imm, 0);
}

void rec_JAL(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	lightrec_emit_end_of_block(_jit, 0, op.j.imm, pc + 8);
}

static void rec_b(jit_state_t *_jit, union opcode op,
		u32 pc, jit_code_t code)
{
	u8 rs, rt;
	jit_node_t *addr;

	jit_note(__FILE__, __LINE__);
	rs = lightrec_alloc_reg_in(_jit, op.i.rs);
	rt = lightrec_alloc_reg_in(_jit, op.i.rt);

	addr = jit_new_node_pww(code, NULL, rs, rt);
	lightrec_emit_end_of_block(_jit, 0,
			pc + 4 + (s16) (op.i.imm << 2), 0);
	jit_patch(addr);

	lightrec_free_regs();
}

static void rec_bz(jit_state_t *_jit, union opcode op,
		u32 pc, jit_code_t code, u32 link)
{
	u8 rs;
	jit_node_t *addr;

	jit_note(__FILE__, __LINE__);
	rs = lightrec_alloc_reg_in(_jit, op.i.rs);

	addr = jit_new_node_pww(code, NULL, rs, 0);
	lightrec_emit_end_of_block(_jit, 0,
			pc + 4 + (s16) (op.i.imm << 2), link);
	jit_patch(addr);

	lightrec_free_regs();
}

void rec_BNE(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	rec_b(_jit, op, pc, jit_code_beqr);
}

void rec_BEQ(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	rec_b(_jit, op, pc, jit_code_bner);
}

void rec_BLEZ(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	rec_bz(_jit, op, pc, jit_code_bgti, 0);
}

void rec_BGTZ(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	rec_bz(_jit, op, pc, jit_code_blei, 0);
}

void rec_regimm_BLTZ(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	rec_bz(_jit, op, pc, jit_code_bgei, 0);
}

void rec_regimm_BLTZAL(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	rec_bz(_jit, op, pc, jit_code_bgei, pc + 8);
}

void rec_regimm_BGEZ(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	rec_bz(_jit, op, pc, jit_code_blti, 0);
}

void rec_regimm_BGEZAL(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	rec_bz(_jit, op, pc, jit_code_blti, pc + 8);
}

static void rec_alu_imm(jit_state_t *_jit, union opcode op,
		jit_code_t code, bool sign_extend)
{
	u8 rs = lightrec_alloc_reg_in(_jit, op.i.rs),
	   rt = lightrec_alloc_reg_out(_jit, op.i.rt);

	jit_note(__FILE__, __LINE__);
	if (sign_extend)
		jit_new_node_www(code, rt, rs, (s32)(s16) op.i.imm);
	else
		jit_new_node_www(code, rt, rs, (u32)(u16) op.i.imm);

	lightrec_free_regs();
}

static void rec_alu_special(jit_state_t *_jit, union opcode op,
		jit_code_t code, bool invert_rs_rt)
{
	u8 rs = lightrec_alloc_reg_in(_jit, op.r.rs),
	   rt = lightrec_alloc_reg_in(_jit, op.r.rt),
	   rd = lightrec_alloc_reg_out(_jit, op.r.rd);

	jit_note(__FILE__, __LINE__);
	if (invert_rs_rt)
		jit_new_node_www(code, rd, rt, rs);
	else
		jit_new_node_www(code, rd, rs, rt);

	lightrec_free_regs();
}

void rec_ADDIU(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	rec_alu_imm(_jit, op, jit_code_addi, false);
}

void rec_ADDI(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	/* TODO: Handle the exception? */
	jit_name(__func__);
	rec_alu_imm(_jit, op, jit_code_addi, false);
}

void rec_SLTIU(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	rec_alu_imm(_jit, op, jit_code_lti_u, true);
}

void rec_SLTI(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	rec_alu_imm(_jit, op, jit_code_lti, true);
}

void rec_ANDI(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	rec_alu_imm(_jit, op, jit_code_andi, false);
}

void rec_ORI(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	rec_alu_imm(_jit, op, jit_code_ori, false);
}

void rec_XORI(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	rec_alu_imm(_jit, op, jit_code_xori, false);
}

void rec_special_ADDU(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	rec_alu_special(_jit, op, jit_code_addr, false);
}

void rec_special_ADD(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	/* TODO: Handle the exception? */
	jit_name(__func__);
	rec_alu_special(_jit, op, jit_code_addr, false);
}

void rec_special_SUBU(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	rec_alu_special(_jit, op, jit_code_subr, false);
}

void rec_special_SUB(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	/* TODO: Handle the exception? */
	jit_name(__func__);
	rec_alu_special(_jit, op, jit_code_subr, false);
}

void rec_special_AND(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	rec_alu_special(_jit, op, jit_code_andr, false);
}

void rec_special_OR(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	rec_alu_special(_jit, op, jit_code_orr, false);
}

void rec_special_XOR(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	rec_alu_special(_jit, op, jit_code_xorr, false);
}

void rec_special_NOR(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	rec_alu_special(_jit, op, jit_code_xorr, false);
	u8 rs, rt, rd;

	jit_name(__func__);
	rs = lightrec_alloc_reg_in(_jit, op.r.rs);
	rt = lightrec_alloc_reg_in(_jit, op.r.rt);
	rd = lightrec_alloc_reg_out(_jit, op.r.rd);

	jit_note(__FILE__, __LINE__);
	jit_orr(rd, rt, rs);
	jit_comr(rd, rd);

	lightrec_free_regs();
}

void rec_special_SLTU(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	rec_alu_special(_jit, op, jit_code_ltr_u, false);
}

void rec_special_SLT(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	rec_alu_special(_jit, op, jit_code_ltr, false);
}

void rec_special_SLLV(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	rec_alu_special(_jit, op, jit_code_lshr, true);
}

void rec_special_SRLV(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	rec_alu_special(_jit, op, jit_code_rshr_u, true);
}

void rec_special_SRAV(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	rec_alu_special(_jit, op, jit_code_rshr, true);
}

static void rec_alu_shift(jit_state_t *_jit, union opcode op,
		jit_code_t code)
{
	u8 rt = lightrec_alloc_reg_in(_jit, op.r.rt),
	   rd = lightrec_alloc_reg_out(_jit, op.r.rd);

	jit_note(__FILE__, __LINE__);
	jit_new_node_www(code, rd, rt, op.r.imm);

	lightrec_free_regs();
}

void rec_special_SLL(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	rec_alu_shift(_jit, op, jit_code_lshi);
}

void rec_special_SRL(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	rec_alu_shift(_jit, op, jit_code_rshi_u);
}

void rec_special_SRA(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	rec_alu_shift(_jit, op, jit_code_rshi);
}
