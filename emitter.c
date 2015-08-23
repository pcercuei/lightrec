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

int emit_call_to_interpreter(const struct block *block,
		struct opcode *op, u32 pc)
{
	/* TODO: Generate something... */
	WARNING("Opcode not compiled: 0x%08x\n", op->opcode);
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

static struct opcode * find_delay_slot(const struct block *block, u32 pc)
{
	struct opcode *elm = block->opcode_list;
	u32 pc_addr;

	for (pc_addr = block->pc; pc_addr <= pc; pc_addr += 4)
		elm = SLIST_NEXT(elm, next);
	return elm;
}

static int lightrec_emit_end_of_block(const struct block *block, u32 pc,
		u8 reg_new_pc, u32 imm, u32 link, struct opcode *delay_slot)
{
	u32 offset, cycles = block->cycles;
	jit_state_t *_jit = block->_jit;

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
		cycles += lightrec_cycles_of_opcode(delay_slot);

		/* Recompile the delay slot */
		if (delay_slot->opcode)
			lightrec_rec_opcode(block, delay_slot, pc + 4);
	}

	lightrec_storeback_regs(_jit);
	lightrec_unlink_addresses();

	/* Increment the cycle counter */
	jit_ldxi_i(JIT_R0, LIGHTREC_REG_STATE,
			offsetof(struct lightrec_state, block_exit_cycles));
	jit_addi(JIT_R0, JIT_R0, cycles);
	jit_stxi_i(offsetof(struct lightrec_state, block_exit_cycles),
			LIGHTREC_REG_STATE, JIT_R0);

	jit_prepare();
	jit_pushargr(LIGHTREC_REG_STATE);
	jit_finishi(&__get_jump_address_cb);
	jit_retval(JIT_R0);
	jit_jmpr(JIT_R0);
	return SKIP_DELAY_SLOT;
}

int rec_special_JR(const struct block *block, struct opcode *op, u32 pc)
{
	struct opcode *elm = find_delay_slot(block, pc);
	u8 rs = lightrec_alloc_reg_in(block->_jit, op->r.rs);

	_jit_name(block->_jit, __func__);
	return lightrec_emit_end_of_block(block, pc, rs, 0, 0, elm);
}

int rec_special_JALR(const struct block *block, struct opcode *op, u32 pc)
{
	struct opcode *elm = find_delay_slot(block, pc);
	u8 rs = lightrec_alloc_reg_in(block->_jit, op->r.rs);

	_jit_name(block->_jit, __func__);
	return lightrec_emit_end_of_block(block, pc, rs, 0, pc + 8, elm);
}

int rec_J(const struct block *block, struct opcode *op, u32 pc)
{
	struct opcode *elm = find_delay_slot(block, pc);

	_jit_name(block->_jit, __func__);
	return lightrec_emit_end_of_block(block, pc, 0,
			(pc & 0xf0000000) | (op->j.imm << 2), 0, elm);
}

int rec_JAL(const struct block *block, struct opcode *op, u32 pc)
{
	struct opcode *elm = find_delay_slot(block, pc);

	_jit_name(block->_jit, __func__);
	return lightrec_emit_end_of_block(block, pc, 0,
			(pc & 0xf0000000) | (op->j.imm << 2), pc + 8, elm);
}

static void preload_in_regs(jit_state_t *_jit, struct opcode *op)
{
	switch (op->i.op) {
	case OP_META:
		switch (op->r.op) {
		case OP_META_SB ... OP_META_SW:
			if (op->i.rt) {
				u8 reg = lightrec_alloc_reg_in(_jit, op->i.rt);
				lightrec_free_reg(reg);
			}
		case OP_META_LB ... OP_META_LW:
			if (op->i.rs) {
				u8 reg = lightrec_alloc_reg_in(_jit, op->i.rs);
				lightrec_free_reg(reg);
			}

			/* Force storeback of the JIT_R0 register, in case we have a
			 * load/store in the delay slot */
			lightrec_clean_reg(_jit, JIT_R0);
			break;
		}
		break;

	case OP_LB ... OP_SWR:
		lightrec_clean_regs(_jit);
		break;

	case OP_SPECIAL:
	case OP_BEQ:
	case OP_BNE:
		if (op->i.rt) {
			u8 reg = lightrec_alloc_reg_in(_jit, op->i.rt);
			lightrec_free_reg(reg);
		}
	default:
		if (op->i.rs) {
			u8 reg = lightrec_alloc_reg_in(_jit, op->i.rs);
			lightrec_free_reg(reg);
		}
	case OP_LUI:
	case OP_J:
	case OP_JAL:
		break;
	}
}

static int rec_b(const struct block *block, struct opcode *op, u32 pc,
		jit_code_t code, u32 link, bool unconditional, bool bz)
{
	struct opcode *delay_slot = find_delay_slot(block, pc);
	jit_state_t *_jit = block->_jit;
	jit_node_t *addr;

	jit_note(__FILE__, __LINE__);

	if (delay_slot->opcode)
		preload_in_regs(_jit, delay_slot);

	if (!unconditional) {
		u8 rs = lightrec_alloc_reg_in(_jit, op->i.rs),
		   rt = bz ? 0 : lightrec_alloc_reg_in(_jit, op->i.rt);
		addr = jit_new_node_pww(code, NULL, rs, rt);

		lightrec_free_reg(rs);
		if (!bz)
			lightrec_free_reg(rt);
	}

	lightrec_emit_end_of_block(block, pc, 0,
			pc + 4 + (s16) (op->i.imm << 2), link, delay_slot);

	if (!unconditional) {
		jit_patch(addr);

		if (delay_slot->opcode /* TODO: BL opcodes */)
			lightrec_rec_opcode(block, delay_slot, pc + 4);
	}

	return SKIP_DELAY_SLOT;
}

int rec_BNE(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_b(block, op, pc, jit_code_beqr, 0, false, false);
}

int rec_BEQ(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_b(block, op, pc, jit_code_bner, 0,
			op->i.rs == op->i.rt, false);
}

int rec_BLEZ(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_b(block, op, pc, jit_code_bgti, 0,
			op->i.rs == 0, true);
}

int rec_BGTZ(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_b(block, op, pc, jit_code_blei, 0, false, true);
}

int rec_regimm_BLTZ(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_b(block, op, pc, jit_code_bgei, 0, false, true);
}

int rec_regimm_BLTZAL(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_b(block, op, pc, jit_code_bgei, pc + 8, false, true);
}

int rec_regimm_BGEZ(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_b(block, op, pc, jit_code_blti, 0, !op->i.rs, true);
}

int rec_regimm_BGEZAL(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_b(block, op, pc, jit_code_blti,
			pc + 8, !op->i.rs, true);
}

static int rec_alu_imm(const struct block *block, struct opcode *op,
		jit_code_t code, bool sign_extend)
{
	jit_state_t *_jit = block->_jit;
	u8 rs = lightrec_alloc_reg_in(_jit, op->i.rs),
	   rt = lightrec_alloc_reg_out(_jit, op->i.rt);

	jit_note(__FILE__, __LINE__);
	if (sign_extend)
		jit_new_node_www(code, rt, rs, (s32)(s16) op->i.imm);
	else
		jit_new_node_www(code, rt, rs, (u32)(u16) op->i.imm);

	lightrec_free_reg(rs);
	lightrec_free_reg(rt);
	return 0;
}

static int rec_alu_special(const struct block *block, struct opcode *op,
		jit_code_t code, bool invert_rs_rt)
{
	jit_state_t *_jit = block->_jit;
	u8 rs = lightrec_alloc_reg_in(_jit, op->r.rs),
	   rt = lightrec_alloc_reg_in(_jit, op->r.rt),
	   rd = lightrec_alloc_reg_out(_jit, op->r.rd);

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

int rec_ADDIU(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_alu_imm(block, op, jit_code_addi, true);
}

int rec_ADDI(const struct block *block, struct opcode *op, u32 pc)
{
	/* TODO: Handle the exception? */
	_jit_name(block->_jit, __func__);
	return rec_alu_imm(block, op, jit_code_addi, true);
}

int rec_SLTIU(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_alu_imm(block, op, jit_code_lti_u, true);
}

int rec_SLTI(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_alu_imm(block, op, jit_code_lti, true);
}

int rec_ANDI(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_alu_imm(block, op, jit_code_andi, false);
}

int rec_ORI(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_alu_imm(block, op, jit_code_ori, false);
}

int rec_XORI(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_alu_imm(block, op, jit_code_xori, false);
}

int rec_LUI(const struct block *block, struct opcode *op, u32 pc)
{
	jit_state_t *_jit = block->_jit;
	u8 rt;

	jit_name(__func__);
	rt = lightrec_alloc_reg_out(_jit, op->i.rt);

	jit_note(__FILE__, __LINE__);
	jit_movi(rt, op->i.imm << 16);

	lightrec_free_reg(rt);
	return 0;
}

int rec_special_ADDU(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_alu_special(block, op, jit_code_addr, false);
}

int rec_special_ADD(const struct block *block, struct opcode *op, u32 pc)
{
	/* TODO: Handle the exception? */
	_jit_name(block->_jit, __func__);
	return rec_alu_special(block, op, jit_code_addr, false);
}

int rec_special_SUBU(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_alu_special(block, op, jit_code_subr, false);
}

int rec_special_SUB(const struct block *block, struct opcode *op, u32 pc)
{
	/* TODO: Handle the exception? */
	_jit_name(block->_jit, __func__);
	return rec_alu_special(block, op, jit_code_subr, false);
}

int rec_special_AND(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_alu_special(block, op, jit_code_andr, false);
}

int rec_special_OR(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_alu_special(block, op, jit_code_orr, false);
}

int rec_special_XOR(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_alu_special(block, op, jit_code_xorr, false);
}

int rec_special_NOR(const struct block *block, struct opcode *op, u32 pc)
{
	jit_state_t *_jit = block->_jit;
	u8 rd;

	jit_name(__func__);
	rec_alu_special(block, op, jit_code_orr, false);
	rd = lightrec_alloc_reg_out(_jit, op->r.rd);

	jit_note(__FILE__, __LINE__);
	jit_comr(rd, rd);

	lightrec_free_reg(rd);
	return 0;
}

int rec_special_SLTU(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_alu_special(block, op, jit_code_ltr_u, false);
}

int rec_special_SLT(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_alu_special(block, op, jit_code_ltr, false);
}

int rec_special_SLLV(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_alu_special(block, op, jit_code_lshr, true);
}

int rec_special_SRLV(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_alu_special(block, op, jit_code_rshr_u, true);
}

int rec_special_SRAV(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_alu_special(block, op, jit_code_rshr, true);
}

static int rec_alu_shift(const struct block *block,
		struct opcode *op, jit_code_t code)
{
	jit_state_t *_jit = block->_jit;
	u8 rt = lightrec_alloc_reg_in(_jit, op->r.rt),
	   rd = lightrec_alloc_reg_out(_jit, op->r.rd);

	jit_note(__FILE__, __LINE__);
	jit_new_node_www(code, rd, rt, op->r.imm);

	lightrec_free_reg(rt);
	lightrec_free_reg(rd);
	return 0;
}

int rec_special_SLL(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_alu_shift(block, op, jit_code_lshi);
}

int rec_special_SRL(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_alu_shift(block, op, jit_code_rshi_u);
}

int rec_special_SRA(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_alu_shift(block, op, jit_code_rshi);
}

static int rec_alu_mult_div(const struct block *block,
		struct opcode *op, jit_code_t code)
{
	jit_state_t *_jit = block->_jit;
	u8 rs = lightrec_alloc_reg_in(_jit, op->r.rs),
	   rt = lightrec_alloc_reg_in(_jit, op->r.rt),
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

int rec_special_MULT(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_alu_mult_div(block, op, jit_code_qmulr);
}

int rec_special_MULTU(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_alu_mult_div(block, op, jit_code_qmulr_u);
}

int rec_special_DIV(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_alu_mult_div(block, op, jit_code_qdivr);
}

int rec_special_DIVU(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_alu_mult_div(block, op, jit_code_qdivr_u);
}

static int rec_alu_mv_lo_hi(const struct block *block, u8 dst, u8 src)
{
	jit_state_t *_jit = block->_jit;
	src = lightrec_alloc_reg_in(_jit, src);
	dst = lightrec_alloc_reg_out(_jit, dst);

	jit_note(__FILE__, __LINE__);
	jit_movr(dst, src);

	lightrec_free_reg(src);
	lightrec_free_reg(dst);
	return 0;
}

int rec_special_MFHI(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_alu_mv_lo_hi(block, op->r.rd, REG_HI);
}

int rec_special_MTHI(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_alu_mv_lo_hi(block, REG_HI, op->r.rs);
}

int rec_special_MFLO(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_alu_mv_lo_hi(block, op->r.rd, REG_LO);
}

int rec_special_MTLO(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_alu_mv_lo_hi(block, REG_LO, op->r.rs);
}

static int rec_store(const struct block *block, struct opcode *op)
{
	jit_state_t *_jit = block->_jit;
	u8 rt, rs;

	jit_note(__FILE__, __LINE__);

	jit_prepare();
	jit_pushargr(LIGHTREC_REG_STATE);
	jit_pushargi(op);

	rs = lightrec_alloc_reg_in(_jit, op->i.rs);
	jit_pushargr(rs);
	lightrec_free_reg(rs);

	rt = lightrec_alloc_reg_in(_jit, op->i.rt);
	jit_pushargr(rt);
	lightrec_free_reg(rt);

	lightrec_storeback_regs(_jit);

	/* The call to C trashes the registers, we have to reset the cache */
	lightrec_regcache_reset();

	jit_finishi(block->state->rw_op);
	return 0;
}

static int rec_store_meta(const struct block *block,
		struct opcode *op, jit_code_t code)
{
	jit_state_t *_jit = block->_jit;
	u8 rt, rs;

	rs = lightrec_alloc_reg_in_address(_jit, op->i.rs, (s16) op->i.imm);
	rt = lightrec_alloc_reg_in(_jit, op->i.rt);

	jit_new_node_www(code, (s16) op->i.imm, rs, rt);

	lightrec_free_reg(rt);
	lightrec_free_reg(rs);
	return 0;
}

static int rec_load(const struct block *block, struct opcode *op)
{
	jit_state_t *_jit = block->_jit;
	u8 rt, rs;

	jit_note(__FILE__, __LINE__);

	jit_prepare();
	jit_pushargr(LIGHTREC_REG_STATE);
	jit_pushargi(op);

	rs = lightrec_alloc_reg_in(_jit, op->i.rs);
	jit_pushargr(rs);
	lightrec_free_reg(rs);

	lightrec_storeback_regs(_jit);

	/* The call to C trashes the registers, we have to reset the cache */
	lightrec_regcache_reset();

	jit_finishi(block->state->rw_op);

	rt = lightrec_alloc_reg_out(_jit, op->i.rt);
	jit_retval_i(rt);
	lightrec_free_reg(rt);

	return 0;
}

static int rec_load_meta(const struct block *block,
		struct opcode *op, jit_code_t code)
{
	jit_state_t *_jit = block->_jit;
	u8 rt, rs;

	rs = lightrec_alloc_reg_in_address(_jit, op->i.rs, (s16) op->i.imm);
	rt = lightrec_alloc_reg_out(_jit, op->i.rt);

	jit_new_node_www(code, rt, rs, (s16) op->i.imm);

	lightrec_free_reg(rt);
	lightrec_free_reg(rs);
	return 0;
}

int rec_SB(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_store(block, op);
}

int rec_SH(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_store(block, op);
}

int rec_SW(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_store(block, op);
}

int rec_SWL(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_store(block, op);
}

int rec_SWR(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_store(block, op);
}

int rec_meta_SB(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_store_meta(block, op, jit_code_stxi_c);
}

int rec_meta_SH(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_store_meta(block, op, jit_code_stxi_s);
}

int rec_meta_SW(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_store_meta(block, op, jit_code_stxi_i);
}

int rec_LB(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_load(block, op);
}

int rec_LBU(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_load(block, op);
}

int rec_LH(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_load(block, op);
}

int rec_LHU(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_load(block, op);
}

int rec_LWL(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_load(block, op);
}

int rec_LWR(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_load(block, op);
}

int rec_LW(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_load(block, op);
}

int rec_meta_LB(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_load_meta(block, op, jit_code_ldxi_c);
}

int rec_meta_LBU(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_load_meta(block, op, jit_code_ldxi_uc);
}

int rec_meta_LH(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_load_meta(block, op, jit_code_ldxi_s);
}

int rec_meta_LHU(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_load_meta(block, op, jit_code_ldxi_us);
}

int rec_meta_LW(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return rec_load_meta(block, op, jit_code_ldxi_i);
}

int rec_special_SYSCALL(const struct block *block, struct opcode *op, u32 pc)
{
	jit_state_t *_jit = block->_jit;
	u8 tmp = lightrec_alloc_reg_temp(_jit);
	u32 offset = offsetof(struct lightrec_state, block_exit_flags);

	jit_name(__func__);
	jit_movi(tmp, LIGHTREC_EXIT_SYSCALL);
	jit_stxi_i(offset, LIGHTREC_REG_STATE, tmp);

	/* TODO: the return address should be "pc - 4" if we're a delay slot */
	return lightrec_emit_end_of_block(block, pc, 0, pc, 0, NULL);
}

int rec_special_BREAK(const struct block *block, struct opcode *op, u32 pc)
{
	jit_state_t *_jit = block->_jit;
	u8 tmp = lightrec_alloc_reg_temp(_jit);
	u32 offset = offsetof(struct lightrec_state, block_exit_flags);

	jit_name(__func__);
	jit_movi(tmp, LIGHTREC_EXIT_BREAK);
	jit_stxi_i(offset, LIGHTREC_REG_STATE, tmp);

	/* TODO: the return address should be "pc - 4" if we're a delay slot */
	return lightrec_emit_end_of_block(block, pc, 0, pc, 0, NULL);
}

static int lightrec_mfc(const struct block *block,
		struct opcode *op, int cpX, bool copy)
{
	u8 rt;
	struct lightrec_state *state = block->state;
	const struct lightrec_cop_ops *ops = state->cop_ops;
	jit_state_t *_jit = block->_jit;

	if (!ops) {
		WARNING("Missing coprocessor callbacks\n");
		return 0;
	}

	if ((copy && !ops->cfc) || (!copy && !ops->mfc)) {
		WARNING("Missing %s callback\n", copy ? "CFC" : "MFC");
		return 0;
	}

	jit_note(__FILE__, __LINE__);

	jit_prepare();
	jit_pushargr(LIGHTREC_REG_STATE);
	jit_pushargi(cpX);
	jit_pushargi(op->r.rd);

	lightrec_storeback_regs(_jit);

	/* The call to C trashes the registers, we have to reset the cache */
	lightrec_regcache_reset();

	jit_finishi(copy ? ops->cfc : ops->mfc);

	rt = lightrec_alloc_reg_out(_jit, op->r.rt);
	jit_retval_i(rt);
	lightrec_free_reg(rt);
	return 0;
}

static int lightrec_mtc(const struct block *block,
		struct opcode *op, int cpX, bool copy)
{
	u8 rt;
	struct lightrec_state *state = block->state;
	const struct lightrec_cop_ops *ops = state->cop_ops;
	jit_state_t *_jit = block->_jit;

	if (!ops) {
		WARNING("Missing coprocessor callbacks\n");
		return 0;
	}

	if ((copy && !ops->ctc) || (!copy && !ops->mtc)) {
		WARNING("Missing %s callback\n", copy ? "CTC" : "MTC");
		return 0;
	}

	rt = lightrec_alloc_reg_in(_jit, op->r.rt);

	jit_note(__FILE__, __LINE__);

	jit_prepare();
	jit_pushargr(LIGHTREC_REG_STATE);
	jit_pushargi(cpX);
	jit_pushargi(op->r.rd);
	jit_pushargr(rt);
	lightrec_free_reg(rt);

	lightrec_storeback_regs(_jit);

	/* The call to C trashes the registers, we have to reset the cache */
	lightrec_regcache_reset();

	jit_finishi(copy ? ops->ctc : ops->mtc);
	return 0;
}

int rec_cp0_MFC0(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return lightrec_mfc(block, op, 0, false);
}

int rec_cp0_CFC0(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return lightrec_mfc(block, op, 0, true);
}

int rec_cp0_MTC0(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return lightrec_mtc(block, op, 0, false);
}

int rec_cp0_CTC0(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return lightrec_mtc(block, op, 0, true);
}

int rec_cp2_basic_MFC2(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return lightrec_mfc(block, op, 2, false);
}

int rec_cp2_basic_CFC2(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return lightrec_mfc(block, op, 2, true);
}

int rec_cp2_basic_MTC2(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return lightrec_mtc(block, op, 2, false);
}

int rec_cp2_basic_CTC2(const struct block *block, struct opcode *op, u32 pc)
{
	_jit_name(block->_jit, __func__);
	return lightrec_mtc(block, op, 2, true);
}

static void rec_cp0_RFE_C(struct lightrec_state *state)
{
	u32 status;

	if (!state->cop_ops || !state->cop_ops->mfc || !state->cop_ops->ctc) {
		WARNING("Missing coprocessor callbacks\n");
		return;
	}

	/* Read CP0 Status register (r12) */
	status = state->cop_ops->mfc(state, 0, 12);

	/* Switch the bits */
	status = ((status & 0x3c) >> 2) | (status & ~0xf);

	/* Write it back */
	state->cop_ops->ctc(state, 0, 12, status);
}

int rec_cp0_RFE(const struct block *block, struct opcode *op, u32 pc)
{
	jit_state_t *_jit = block->_jit;

	jit_name(__func__);

	lightrec_storeback_regs(_jit);

	/* The call to C trashes the registers, we have to reset the cache */
	lightrec_regcache_reset();

	jit_note(__FILE__, __LINE__);
	jit_prepare();
	jit_pushargr(LIGHTREC_REG_STATE);
	jit_finishi(rec_cp0_RFE_C);
	return 0;
}
