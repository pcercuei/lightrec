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

static uintptr_t __get_jump_address_cb(struct lightrec_state *state)
{
	struct block *new;

	if (state->stop)
		return state->end_of_block;

	new = lightrec_find_block(state->block_cache, state->next_pc);
	if (!new) {
		new = lightrec_recompile_block(state, state->next_pc);
		if (!new)
			return state->end_of_block;
		lightrec_register_block(state->block_cache, new);
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

static int lightrec_emit_end_of_block(jit_state_t *_jit,
		const struct block *block, u32 pc,
		u8 reg_new_pc, u32 imm, u32 link,
		struct opcode_list *delay_slot)
{
	u32 offset, cycles = block->cycles;

	jit_note(__FILE__, __LINE__);

	if (link) {
		/* Update the $ra register */
		u8 link_reg = lightrec_alloc_reg_out(_jit, 31);
		jit_movi(link_reg, link);
		lightrec_free_reg(link_reg);
	}

	/* Store the next PC in the lightrec_state structure,
	 * in case we exit the dynarec after this block */
	offset = offsetof(struct lightrec_state, next_pc);
	if (reg_new_pc) {
		jit_stxi_i(offset, LIGHTREC_REG_STATE, reg_new_pc);
	} else {
		u8 tmp = lightrec_alloc_reg_temp(_jit);
		jit_movi(tmp, imm);
		jit_stxi_i(offset, LIGHTREC_REG_STATE, tmp);
		lightrec_free_reg(tmp);
	}

	if (delay_slot) {
		union opcode op = delay_slot->opcode;

		cycles += lightrec_cycles_of_opcode(op);

		/* Recompile the delay slot */
		if (op.opcode)
			lightrec_rec_opcode(_jit, op, block, pc + 4);
	}

	lightrec_storeback_regs(_jit);
	lightrec_unlink_addresses();

	/* Increment the cycle counter */
	jit_ldxi_i(JIT_R0, LIGHTREC_REG_STATE,
			offsetof(struct lightrec_state, block_exit_cycles));
	jit_addi(JIT_R0, JIT_R0, cycles);
	jit_stxi_i(offsetof(struct lightrec_state, block_exit_cycles),
			LIGHTREC_REG_STATE, JIT_R0);

	/* Load the address of lightrec_state in JIT_RA0
	 * FIXME: may not work on all architectures */
	jit_movr(JIT_RA0, LIGHTREC_REG_STATE);

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

static void preload_in_regs(jit_state_t *_jit, union opcode op)
{
	switch (op.i.op) {
	case OP_SPECIAL:
	case OP_BEQ:
	case OP_BNE:
	case OP_SB:
	case OP_SH:
	case OP_SWL:
	case OP_SW:
	case OP_SWR:
		if (op.i.rt) {
			u8 reg = lightrec_alloc_reg_in(_jit, op.i.rt);
			lightrec_free_reg(reg);
		}
	default:
		if (op.i.rs) {
			u8 reg = lightrec_alloc_reg_in(_jit, op.i.rs);
			lightrec_free_reg(reg);
		}
	case OP_LUI:
	case OP_J:
	case OP_JAL:
		break;
	}
}

static int rec_b(jit_state_t *_jit, union opcode op, const struct block *block,
		u32 pc, jit_code_t code, u32 link, bool unconditional, bool bz)
{
	struct opcode_list *delay_slot = find_delay_slot(block, pc);
	jit_node_t *addr;

	jit_note(__FILE__, __LINE__);
	if (delay_slot->opcode.opcode)
		preload_in_regs(_jit, delay_slot->opcode);

	if (!unconditional) {
		u8 rs = lightrec_alloc_reg_in(_jit, op.i.rs),
		   rt = bz ? 0 : lightrec_alloc_reg_in(_jit, op.i.rt);
		addr = jit_new_node_pww(code, NULL, rs, rt);

		lightrec_free_reg(rs);
		if (!bz)
			lightrec_free_reg(rt);
	}

	lightrec_emit_end_of_block(_jit, block, pc, 0,
			pc + 4 + (s16) (op.i.imm << 2), link, delay_slot);

	if (!unconditional) {
		jit_patch(addr);

		if (delay_slot->opcode.opcode /* TODO: BL opcodes */)
			lightrec_rec_opcode(_jit, delay_slot->opcode,
					block, pc + 4);
	}

	return SKIP_DELAY_SLOT;
}

int rec_BNE(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_b(_jit, op, block, pc, jit_code_beqr, 0, false, false);
}

int rec_BEQ(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_b(_jit, op, block, pc, jit_code_bner, 0,
			op.i.rs == op.i.rt, false);
}

int rec_BLEZ(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_b(_jit, op, block, pc, jit_code_bgti, 0,
			op.i.rs == 0, true);
}

int rec_BGTZ(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_b(_jit, op, block, pc, jit_code_blei, 0, false, true);
}

int rec_regimm_BLTZ(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_b(_jit, op, block, pc, jit_code_bgei, 0, false, true);
}

int rec_regimm_BLTZAL(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_b(_jit, op, block, pc, jit_code_bgei, pc + 8, false, true);
}

int rec_regimm_BGEZ(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_b(_jit, op, block, pc, jit_code_blti, 0, op.i.rs == 0, true);
}

int rec_regimm_BGEZAL(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	jit_name(__func__);
	return rec_b(_jit, op, block, pc, jit_code_blti, pc + 8,
			op.i.rs == 0, true);
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

	lightrec_free_reg(rs);
	lightrec_free_reg(rt);
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

	lightrec_free_reg(rs);
	lightrec_free_reg(rt);
	lightrec_free_reg(rd);
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

	lightrec_free_reg(rt);
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
	u8 rd;

	jit_name(__func__);
	rec_alu_special(_jit, op, jit_code_orr, false);
	rd = lightrec_alloc_reg_out(_jit, op.r.rd);

	jit_note(__FILE__, __LINE__);
	jit_comr(rd, rd);

	lightrec_free_reg(rd);
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

	lightrec_free_reg(rt);
	lightrec_free_reg(rd);
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

	lightrec_free_reg(rs);
	lightrec_free_reg(rt);
	lightrec_free_reg(lo);
	lightrec_free_reg(hi);
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

	lightrec_free_reg(src);
	lightrec_free_reg(dst);
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
	rs = lightrec_alloc_reg_in_address(_jit, op.i.rs, (s16) op.i.imm);
	rt = lightrec_alloc_reg_in(_jit, op.i.rt);

	jit_new_node_www(code, (s16) op.i.imm, rs, rt);

	lightrec_free_reg(rt);
	lightrec_free_reg(rs);
	return 0;
}

static int rec_load(jit_state_t *_jit, union opcode op, jit_code_t code)
{
	u8 rt, rs;

	jit_name(__func__);
	rs = lightrec_alloc_reg_in_address(_jit, op.i.rs, (s16) op.i.imm);
	rt = lightrec_alloc_reg_out(_jit, op.i.rt);

	jit_new_node_www(code, rt, rs, (s16) op.i.imm);

	lightrec_free_reg(rt);
	lightrec_free_reg(rs);
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

int rec_special_SYSCALL(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	u8 tmp = lightrec_alloc_reg_temp(_jit);
	u32 offset = offsetof(struct lightrec_state, block_exit_flags);

	jit_name(__func__);
	jit_movi(tmp, LIGHTREC_EXIT_SYSCALL);
	jit_stxi_i(offset, LIGHTREC_REG_STATE, tmp);

	/* TODO: the return address should be "pc - 4" if we're a delay slot */
	return lightrec_emit_end_of_block(_jit, block, pc, 0, pc, 0, NULL);
}

int rec_special_BREAK(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	u8 tmp = lightrec_alloc_reg_temp(_jit);
	u32 offset = offsetof(struct lightrec_state, block_exit_flags);

	jit_name(__func__);
	jit_movi(tmp, LIGHTREC_EXIT_BREAK);
	jit_stxi_i(offset, LIGHTREC_REG_STATE, tmp);

	/* TODO: the return address should be "pc - 4" if we're a delay slot */
	return lightrec_emit_end_of_block(_jit, block, pc, 0, pc, 0, NULL);
}
