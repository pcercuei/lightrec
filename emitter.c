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
#include "recompiler.h"
#include "regcache.h"

#include <lightning.h>
#include <stddef.h>

#define REG_LO 32
#define REG_HI 33

int emit_call_to_interpreter(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	/* TODO: Generate something... */
	WARNING("Opcode not compiled: 0x%08x\n", op.opcode);
	return 0;
}

static uintptr_t __get_jump_address_cb(u32 pc)
{
	struct block *new;

	if (lightrec_state->stop)
		return lightrec_state->end_of_block;

	new = lightrec_find_block(pc);
	if (!new) {
		new = lightrec_recompile_block(pc);
		if (!new)
			return lightrec_state->end_of_block;
		lightrec_register_block(new);
	}

	return (uintptr_t) new->function;
}

static struct opcode_list * find_delay_slot(const struct block *block, u32 pc)
{
	struct opcode_list *elm = block->opcode_list;
	u32 pc_addr;

	for (pc_addr = block->pc; pc_addr <= pc; pc_addr += 4)
		elm = SLIST_NEXT(elm, next);
	return elm;
}

static bool delay_slot_trashed_jit_ra0(union opcode op)
{
	switch (op.i.op) {
	case OP_LB:
	case OP_LBU:
	case OP_LH:
	case OP_LHU:
	case OP_LW:
	case OP_LWL:
	case OP_LWR:
	case OP_SB:
	case OP_SH:
	case OP_SW:
		return true;
	default:
		return false;
	};
}

static int lightrec_emit_end_of_block(jit_state_t *_jit,
		const struct block *block, u32 pc,
		u8 reg_new_pc, u32 imm, u32 link,
		struct opcode_list *delay_slot)
{
	u32 offset;

	jit_note(__FILE__, __LINE__);

	if (link) {
		/* Update the $ra register */
		offset = offsetof(struct lightrec_state, reg_cache) + (31 << 2);
		jit_movi(JIT_RA0, link);
		jit_stxi_i(offset, LIGHTREC_REG_STATE, JIT_RA0);
	}

	if (!reg_new_pc)
		jit_movi(JIT_RA0, imm);
	else
		jit_movr(JIT_RA0, reg_new_pc);

	/* Store the next PC in the lightrec_state structure,
	 * in case we exit the dynarec after this block */
	offset = offsetof(struct lightrec_state, next_pc);
	jit_stxi_i(offset, LIGHTREC_REG_STATE, JIT_RA0);

	/* Recompile the delay slot */
	lightrec_rec_opcode(_jit, delay_slot->opcode, block, pc + 4);

	lightrec_storeback_regs(_jit);

	/* Reload the next PC if it's no more in JIT_RA0 */
	if (delay_slot_trashed_jit_ra0(delay_slot->opcode))
		jit_ldxi_i(JIT_RA0, LIGHTREC_REG_STATE, offset);

	/* FIXME: Passing the next PC as parameter in JIT_RA0
	 * may not work on all architectures */
	jit_calli(&__get_jump_address_cb);
	jit_retval(JIT_R0);
	jit_jmpr(JIT_R0);
	return SKIP_DELAY_SLOT;
}

int rec_special_JR(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	struct opcode_list *elm = find_delay_slot(block, pc);
	u8 rs = lightrec_alloc_reg_in(_jit, op.r.rs);

	jit_name(__func__);
	return lightrec_emit_end_of_block(_jit, block, pc, rs, 0, 0, elm);
}

int rec_special_JALR(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	struct opcode_list *elm = find_delay_slot(block, pc);
	u8 rs = lightrec_alloc_reg_in(_jit, op.r.rs);

	jit_name(__func__);
	return lightrec_emit_end_of_block(_jit, block, pc, rs, 0, pc + 8, elm);
}

int rec_J(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	struct opcode_list *elm = find_delay_slot(block, pc);

	jit_name(__func__);
	return lightrec_emit_end_of_block(_jit, block, pc, 0,
			(pc & 0xf0000000) | (op.j.imm << 2), 0, elm);
}

int rec_JAL(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	struct opcode_list *elm = find_delay_slot(block, pc);

	jit_name(__func__);
	return lightrec_emit_end_of_block(_jit, block, pc, 0,
			(pc & 0xf0000000) | (op.j.imm << 2), pc + 8, elm);
}

static int rec_b(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc, jit_code_t code)
{
	struct opcode_list *delay_slot = find_delay_slot(block, pc);
	u8 rs, rt;
	jit_node_t *addr;

	jit_note(__FILE__, __LINE__);
	rs = lightrec_alloc_reg_in(_jit, op.i.rs);
	rt = lightrec_alloc_reg_in(_jit, op.i.rt);

	addr = jit_new_node_pww(code, NULL, rs, rt);
	lightrec_emit_end_of_block(_jit, block, pc, 0,
			pc + 4 + (s16) (op.i.imm << 2), 0, delay_slot);
	jit_patch(addr);

	if (1 /* TODO: BL opcodes */)
		lightrec_rec_opcode(_jit, delay_slot->opcode, block, pc + 4);

	lightrec_free_regs();
	return SKIP_DELAY_SLOT;
}

static int rec_bz(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc, jit_code_t code, u32 link)
{
	struct opcode_list *delay_slot = find_delay_slot(block, pc);
	u8 rs;
	jit_node_t *addr;

	jit_note(__FILE__, __LINE__);
	rs = lightrec_alloc_reg_in(_jit, op.i.rs);

	addr = jit_new_node_pww(code, NULL, rs, 0);
	lightrec_emit_end_of_block(_jit, block, pc, 0,
			pc + 4 + (s16) (op.i.imm << 2), link, delay_slot);
	jit_patch(addr);

	if (1 /* TODO: BL opcodes */)
		lightrec_rec_opcode(_jit, delay_slot->opcode, block, pc + 4);

	lightrec_free_regs();
	return SKIP_DELAY_SLOT;
}

int rec_BNE(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_b(_jit, op, block, pc, jit_code_beqr);
}

int rec_BEQ(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_b(_jit, op, block, pc, jit_code_bner);
}

int rec_BLEZ(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_bz(_jit, op, block, pc, jit_code_bgti, 0);
}

int rec_BGTZ(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_bz(_jit, op, block, pc, jit_code_blei, 0);
}

int rec_regimm_BLTZ(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_bz(_jit, op, block, pc, jit_code_bgei, 0);
}

int rec_regimm_BLTZAL(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_bz(_jit, op, block, pc, jit_code_bgei, pc + 8);
}

int rec_regimm_BGEZ(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_bz(_jit, op, block, pc, jit_code_blti, 0);
}

int rec_regimm_BGEZAL(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_bz(_jit, op, block, pc, jit_code_blti, pc + 8);
}

static int rec_alu_imm(jit_state_t *_jit, union opcode op,
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
	return 0;
}

static int rec_alu_special(jit_state_t *_jit, union opcode op,
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
	return 0;
}

int rec_ADDIU(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_alu_imm(_jit, op, jit_code_addi, true);
}

int rec_ADDI(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	/* TODO: Handle the exception? */
	jit_name(__func__);
	return rec_alu_imm(_jit, op, jit_code_addi, true);
}

int rec_SLTIU(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_alu_imm(_jit, op, jit_code_lti_u, true);
}

int rec_SLTI(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_alu_imm(_jit, op, jit_code_lti, true);
}

int rec_ANDI(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_alu_imm(_jit, op, jit_code_andi, false);
}

int rec_ORI(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_alu_imm(_jit, op, jit_code_ori, false);
}

int rec_XORI(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_alu_imm(_jit, op, jit_code_xori, false);
}

int rec_LUI(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	u8 rt;

	jit_name(__func__);
	rt = lightrec_alloc_reg_out(_jit, op.i.rt);

	jit_note(__FILE__, __LINE__);
	jit_movi(rt, op.i.imm << 16);

	lightrec_free_regs();
	return 0;
}

int rec_special_ADDU(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_alu_special(_jit, op, jit_code_addr, false);
}

int rec_special_ADD(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	/* TODO: Handle the exception? */
	jit_name(__func__);
	return rec_alu_special(_jit, op, jit_code_addr, false);
}

int rec_special_SUBU(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_alu_special(_jit, op, jit_code_subr, false);
}

int rec_special_SUB(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	/* TODO: Handle the exception? */
	jit_name(__func__);
	return rec_alu_special(_jit, op, jit_code_subr, false);
}

int rec_special_AND(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_alu_special(_jit, op, jit_code_andr, false);
}

int rec_special_OR(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_alu_special(_jit, op, jit_code_orr, false);
}

int rec_special_XOR(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_alu_special(_jit, op, jit_code_xorr, false);
}

int rec_special_NOR(jit_state_t *_jit, union opcode op,
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
	return 0;
}

int rec_special_SLTU(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_alu_special(_jit, op, jit_code_ltr_u, false);
}

int rec_special_SLT(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_alu_special(_jit, op, jit_code_ltr, false);
}

int rec_special_SLLV(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_alu_special(_jit, op, jit_code_lshr, true);
}

int rec_special_SRLV(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_alu_special(_jit, op, jit_code_rshr_u, true);
}

int rec_special_SRAV(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_alu_special(_jit, op, jit_code_rshr, true);
}

static int rec_alu_shift(jit_state_t *_jit, union opcode op,
		jit_code_t code)
{
	u8 rt = lightrec_alloc_reg_in(_jit, op.r.rt),
	   rd = lightrec_alloc_reg_out(_jit, op.r.rd);

	jit_note(__FILE__, __LINE__);
	jit_new_node_www(code, rd, rt, op.r.imm);

	lightrec_free_regs();
	return 0;
}

int rec_special_SLL(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_alu_shift(_jit, op, jit_code_lshi);
}

int rec_special_SRL(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_alu_shift(_jit, op, jit_code_rshi_u);
}

int rec_special_SRA(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_alu_shift(_jit, op, jit_code_rshi);
}

static int rec_alu_mult_div(jit_state_t *_jit, union opcode op,
		jit_code_t code)
{
	u8 rs = lightrec_alloc_reg_in(_jit, op.r.rs),
	   rt = lightrec_alloc_reg_in(_jit, op.r.rt),
	   lo = lightrec_alloc_reg_out(_jit, REG_LO),
	   hi = lightrec_alloc_reg_out(_jit, REG_HI);

	jit_note(__FILE__, __LINE__);
	jit_new_node_qww(code, lo, hi, rs, rt);

	lightrec_free_regs();
	return 0;
}

int rec_special_MULT(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_alu_mult_div(_jit, op, jit_code_qmulr);
}

int rec_special_MULTU(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_alu_mult_div(_jit, op, jit_code_qmulr_u);
}

int rec_special_DIV(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_alu_mult_div(_jit, op, jit_code_qdivr);
}

int rec_special_DIVU(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_alu_mult_div(_jit, op, jit_code_qdivr_u);
}

static int rec_alu_mv_lo_hi(jit_state_t *_jit, u8 dst, u8 src)
{
	src = lightrec_alloc_reg_in(_jit, src);
	dst = lightrec_alloc_reg_out(_jit, dst);

	jit_note(__FILE__, __LINE__);
	jit_movr(dst, src);

	lightrec_free_regs();
	return 0;
}

int rec_special_MFHI(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_alu_mv_lo_hi(_jit, op.r.rd, REG_HI);
}

int rec_special_MTHI(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_alu_mv_lo_hi(_jit, REG_HI, op.r.rs);
}

int rec_special_MFLO(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_alu_mv_lo_hi(_jit, op.r.rd, REG_LO);
}

int rec_special_MTLO(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_alu_mv_lo_hi(_jit, REG_LO, op.r.rs);
}

static int rec_store(jit_state_t *_jit, union opcode op, jit_code_t code)
{
	u8 rt, rs;

	jit_name(__func__);
	rs = lightrec_alloc_reg_in_address(_jit, op.i.rs);
	rt = lightrec_alloc_reg_in(_jit, op.i.rt);

	jit_new_node_www(code, (s16) op.i.imm, rs, rt);

	lightrec_free_regs();
	return 0;
}

static int rec_load(jit_state_t *_jit, union opcode op, jit_code_t code)
{
	u8 rt, rs;

	jit_name(__func__);
	rs = lightrec_alloc_reg_in_address(_jit, op.i.rs);
	rt = lightrec_alloc_reg_out(_jit, op.i.rt);

	jit_new_node_www(code, rt, rs, (s16) op.i.imm);

	lightrec_free_regs();
	return 0;
}

int rec_SB(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	return rec_store(_jit, op, jit_code_stxi_c);
}

int rec_SH(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	return rec_store(_jit, op, jit_code_stxi_s);
}

int rec_SW(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	return rec_store(_jit, op, jit_code_stxi_i);
}

int rec_LB(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	return rec_load(_jit, op, jit_code_ldxi_c);
}

int rec_LBU(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	return rec_load(_jit, op, jit_code_ldxi_uc);
}

int rec_LH(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	return rec_load(_jit, op, jit_code_ldxi_s);
}

int rec_LHU(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	return rec_load(_jit, op, jit_code_ldxi_us);
}

int rec_LW(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	return rec_load(_jit, op, jit_code_ldxi_i);
}
