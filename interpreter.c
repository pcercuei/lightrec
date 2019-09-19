/*
 * Copyright (C) 2019 Paul Cercueil <paul@crapouillou.net>
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

#include "disassembler.h"
#include "interpreter.h"
#include "lightrec-private.h"
#include "optimizer.h"
#include "regcache.h"

#include <stdbool.h>

struct interpreter;

static void int_CP0(struct interpreter *inter);
static void int_CP2(struct interpreter *inter);
static void int_SPECIAL(struct interpreter *inter);
static void int_REGIMM(struct interpreter *inter);

typedef void (*lightrec_int_func_t)(struct interpreter *inter);

static const lightrec_int_func_t int_standard[64];

struct interpreter {
	struct lightrec_state *state;
	struct block *block;
	struct opcode *op;
	u32 pc;
	u32 cycles;
	bool delay_slot;
};

#define EXECUTE(func, inter) do {					\
	(*(func))(inter);						\
	return;								\
} while (0)

#define JUMP_SKIP(inter) do {						\
	inter->op = SLIST_NEXT(inter->op, next);			\
	EXECUTE(int_standard[inter->op->i.op], inter);			\
} while (0)

#define JUMP_NEXT(inter) do {						\
	inter->cycles += lightrec_cycles_of_opcode(inter->op);		\
	if (likely(!inter->delay_slot)) {				\
		inter->op = SLIST_NEXT(inter->op, next);		\
		inter->pc += 4;						\
		EXECUTE(int_standard[inter->op->i.op], inter);		\
	}								\
} while (0)

#define JUMP_AFTER_BRANCH(inter) do {					\
	inter->cycles += lightrec_cycles_of_opcode(inter->op);		\
	inter->op = SLIST_NEXT(SLIST_NEXT(inter->op, next), next);	\
	inter->pc += 8;							\
	EXECUTE(int_standard[inter->op->i.op], inter);			\
} while (0)

static bool load_in_delay_slot(const struct opcode *op)
{
	switch (op->i.op) {
	case OP_CP0:
		switch (op->r.rs) {
		case OP_CP0_MFC0:
		case OP_CP0_CFC0:
			return true;
		default:
			break;
		}

		break;
	case OP_CP2:
		if (op->r.op == OP_CP2_BASIC) {
			switch (op->r.rs) {
			case OP_CP2_BASIC_MFC2:
			case OP_CP2_BASIC_CFC2:
				return true;
			default:
				break;
			}
		}

		break;
	case OP_LWC2:
	case OP_LB:
	case OP_LH:
	case OP_LW:
	case OP_LWL:
	case OP_LWR:
	case OP_LBU:
	case OP_LHU:
		return true;
	default:
		break;
	}

	return false;
}

static u32 int_delay_slot(struct interpreter *inter, u32 pc, bool branch)
{
	u32 *reg_cache = inter->state->native_reg_cache;
	struct opcode *op_next, *op = SLIST_NEXT(inter->op, next);
	struct interpreter inter2 = {
		.state = inter->state,
		.cycles = inter->cycles,
		.delay_slot = true,
	};
	struct block *block;
	bool run_first_op = false, dummy_ld = false, save_rs = false,
	     load_in_ds;
	u32 old_rs, new_rs, new_rt;
	u32 next_pc;

	/* An opcode located in the delay slot performing a delayed read
	 * requires special handling; we will always resort to using the
	 * interpreter in that case. */
	load_in_ds = load_in_delay_slot(op);
	if (load_in_ds)
		inter->block->flags |= BLOCK_NEVER_COMPILE;

	if (branch) {
		if (load_in_ds) {
			block = lightrec_get_block(inter->state, pc);
			op_next = block->opcode_list;

			/* Verify that the next block actually reads the
			 * destination register of the delay slot opcode. */
			run_first_op = opcode_reads_register(op_next, op->r.rt);
		}

		if (load_in_ds && run_first_op) {
			next_pc = pc + 4;

			/* If the first opcode of the next block writes the
			 * regiser used as the address for the load, we need to
			 * reset to the old value after it has been executed,
			 * then restore the new value after the delay slot
			 * opcode has been executed. */
			save_rs = opcode_reads_register(op, op->r.rs) &&
				  opcode_writes_register(op_next, op->r.rs);
			if (save_rs)
				old_rs = reg_cache[op->r.rs];

			/* If both the first opcode of the next block and the
			 * delay slot opcode write to the same register, the
			 * value written by the delay slot opcode is
			 * discarded. */
			dummy_ld = opcode_writes_register(op_next, op->r.rt);

			inter2.block = block;
			inter2.op = op_next;
			inter2.pc = pc;

			/* Execute the first opcode of the next block */
			(*int_standard[inter2.op->i.op])(&inter2);

			if (save_rs) {
				new_rs = reg_cache[op->r.rs];
				reg_cache[op->r.rs] = old_rs;
			}

			inter->cycles += lightrec_cycles_of_opcode(inter2.op);
		} else {
			next_pc = pc;
		}
	} else {
		next_pc = inter->pc + 8;
	}

	inter2.block = inter->block;
	inter2.op = op;
	inter2.pc = inter->pc + 4;
	inter2.cycles = inter->cycles;

	if (dummy_ld)
		new_rt = reg_cache[op->r.rt];

	/* Execute delay slot opcode */
	(*int_standard[inter2.op->i.op])(&inter2);

	if (save_rs)
		reg_cache[op->r.rs] = new_rs;
	if (dummy_ld)
		reg_cache[op->r.rt] = new_rt;

	inter->cycles += lightrec_cycles_of_opcode(op);

	return next_pc;
}

static void int_unimplemented(struct interpreter *inter)
{
	WARNING("Unimplemented opcode 0x%08x\n", inter->op->opcode);

	JUMP_NEXT(inter);
}

static void int_jump(struct interpreter *inter, bool link)
{
	struct lightrec_state *state = inter->state;
	u32 pc = (inter->pc & 0xf0000000) | (inter->op->j.imm << 2);

	if (link)
		state->native_reg_cache[31] = inter->pc + 8;

	pc = int_delay_slot(inter, pc, true);

	inter->pc = pc;
}

static void int_J(struct interpreter *inter)
{
	int_jump(inter, false);
}

static void int_JAL(struct interpreter *inter)
{
	int_jump(inter, true);
}

static void int_jumpr(struct interpreter *inter, u8 link_reg)
{
	struct lightrec_state *state = inter->state;
	u32 next_pc = state->native_reg_cache[inter->op->r.rs];

	if (link_reg)
		state->native_reg_cache[link_reg] = inter->pc + 8;

	next_pc = int_delay_slot(inter, next_pc, true);

	inter->pc = next_pc;
}

static void int_special_JR(struct interpreter *inter)
{
	int_jumpr(inter, 0);
}

static void int_special_JALR(struct interpreter *inter)
{
	int_jumpr(inter, inter->op->r.rd);
}

static void int_beq(struct interpreter *inter, bool bne)
{
	struct lightrec_state *state = inter->state;
	u32 rs, rt, next_pc = inter->pc + 4 + ((s16)inter->op->i.imm << 2);
	bool branch;

	rs = inter->state->native_reg_cache[inter->op->i.rs];
	rt = inter->state->native_reg_cache[inter->op->i.rt];
	branch = (rs == rt) ^ bne;

	next_pc = int_delay_slot(inter, next_pc, branch);

	if (branch)
		inter->pc = next_pc;
	else
		JUMP_AFTER_BRANCH(inter);
}

static void int_BEQ(struct interpreter *inter)
{
	int_beq(inter, false);
}

static void int_BNE(struct interpreter *inter)
{
	int_beq(inter, true);
}

static void int_bgez(struct interpreter *inter, bool link, bool lt, bool regimm)
{
	struct lightrec_state *state = inter->state;
	u32 next_pc = inter->pc + 4 + ((s16)inter->op->i.imm << 2);
	bool branch;
	s32 rs;

	if (link)
		inter->state->native_reg_cache[31] = inter->pc + 8;

	rs = (s32)inter->state->native_reg_cache[inter->op->i.rs];
	branch = (regimm && !rs || rs > 0) ^ lt;

	next_pc = int_delay_slot(inter, next_pc, branch);

	if (branch)
		inter->pc = next_pc;
	else
		JUMP_AFTER_BRANCH(inter);
}

static void int_regimm_BLTZ(struct interpreter *inter)
{
	int_bgez(inter, false, true, true);
}

static void int_regimm_BGEZ(struct interpreter *inter)
{
	int_bgez(inter, false, false, true);
}

static void int_regimm_BLTZAL(struct interpreter *inter)
{
	int_bgez(inter, true, true, true);
}

static void int_regimm_BGEZAL(struct interpreter *inter)
{
	int_bgez(inter, true, false, true);
}

static void int_BLEZ(struct interpreter *inter)
{
	int_bgez(inter, false, true, false);
}

static void int_BGTZ(struct interpreter *inter)
{
	int_bgez(inter, false, false, false);
}

static void int_cfc(struct interpreter *inter)
{
	struct lightrec_state *state = inter->state;
	const struct opcode *op = inter->op;
	u32 val;

	val = lightrec_mfc(state, op->c);

	if (likely(op->r.rt))
		state->native_reg_cache[op->r.rt] = val;

	JUMP_NEXT(inter);
}

static void int_ctc(struct interpreter *inter)
{
	struct lightrec_state *state = inter->state;
	const struct opcode *op = inter->op;

	lightrec_mtc(state, op->c, state->native_reg_cache[op->r.rt]);

	/* If we have a MTC0 or CTC0 to CP0 register 12 (Status) or 13 (Cause),
	 * return early so that the emulator will be able to check software
	 * interrupt status. */
	if (op->i.op == OP_CP0 && (op->r.rd == 12 || op->r.rd == 13))
		inter->pc += 4;
	else
		JUMP_NEXT(inter);
}

static void int_cp0_RFE(struct interpreter *inter)
{
	struct lightrec_state *state = inter->state;
	u32 status;

	/* Read CP0 Status register (r12) */
	status = state->ops.cop0_ops.mfc(state, 12);

	/* Switch the bits */
	status = ((status & 0x3c) >> 2) | (status & ~0xf);

	/* Write it back */
	state->ops.cop0_ops.ctc(state, 12, status);

	JUMP_NEXT(inter);
}

static void int_CP(struct interpreter *inter)
{
	struct lightrec_state *state = inter->state;
	const struct lightrec_cop_ops *ops;
	const struct opcode *op = inter->op;

	if ((op->j.imm >> 25) & 1)
		ops = &state->ops.cop2_ops;
	else
		ops = &state->ops.cop0_ops;

	(*ops->op)(state, (op->j.imm) & ~(1 << 25));

	JUMP_NEXT(inter);
}

static void int_ADDI(struct interpreter *inter)
{
	u32 *reg_cache = inter->state->native_reg_cache;
	struct opcode_i *op = &inter->op->i;

	if (likely(op->rt))
		reg_cache[op->rt] = reg_cache[op->rs] + (s32)(s16)op->imm;

	JUMP_NEXT(inter);
}

static void int_SLTI(struct interpreter *inter)
{
	u32 *reg_cache = inter->state->native_reg_cache;
	struct opcode_i *op = &inter->op->i;

	if (likely(op->rt))
		reg_cache[op->rt] = (s32)reg_cache[op->rs] < (s32)(s16)op->imm;

	JUMP_NEXT(inter);
}

static void int_SLTIU(struct interpreter *inter)
{
	u32 *reg_cache = inter->state->native_reg_cache;
	struct opcode_i *op = &inter->op->i;

	if (likely(op->rt))
		reg_cache[op->rt] = reg_cache[op->rs] < (u32)(s32)(s16)op->imm;

	JUMP_NEXT(inter);
}

static void int_ANDI(struct interpreter *inter)
{
	u32 *reg_cache = inter->state->native_reg_cache;
	struct opcode_i *op = &inter->op->i;

	if (likely(op->rt))
		reg_cache[op->rt] = reg_cache[op->rs] & op->imm;

	JUMP_NEXT(inter);
}

static void int_ORI(struct interpreter *inter)
{
	u32 *reg_cache = inter->state->native_reg_cache;
	struct opcode_i *op = &inter->op->i;

	if (likely(op->rt))
		reg_cache[op->rt] = reg_cache[op->rs] | op->imm;

	JUMP_NEXT(inter);
}

static void int_XORI(struct interpreter *inter)
{
	u32 *reg_cache = inter->state->native_reg_cache;
	struct opcode_i *op = &inter->op->i;

	if (likely(op->rt))
		reg_cache[op->rt] = reg_cache[op->rs] ^ op->imm;

	JUMP_NEXT(inter);
}

static void int_LUI(struct interpreter *inter)
{
	struct opcode_i *op = &inter->op->i;

	inter->state->native_reg_cache[op->rt] = op->imm << 16;

	JUMP_NEXT(inter);
}

static void int_io(struct interpreter *inter, bool is_load)
{
	struct opcode_i *op = &inter->op->i;
	u32 *reg_cache = inter->state->native_reg_cache;
	u32 val;

	val = lightrec_rw(inter->state, inter->op->c,
			  reg_cache[op->rs], reg_cache[op->rt],
			  &inter->op->flags);

	if (is_load && op->rt)
		reg_cache[op->rt] = val;

	JUMP_NEXT(inter);
}

static void int_load(struct interpreter *inter)
{
	int_io(inter, true);
}

static void int_store(struct interpreter *inter)
{
	int_io(inter, false);
}

static void int_LWC2(struct interpreter *inter)
{
	int_io(inter, false);
}

static void int_special_SLL(struct interpreter *inter)
{
	struct opcode *op = inter->op;
	u32 rt;

	if (op->opcode) { /* Handle NOPs */
		rt = inter->state->native_reg_cache[op->r.rt];
		inter->state->native_reg_cache[op->r.rd] = rt << op->r.imm;
	}

	JUMP_NEXT(inter);
}

static void int_special_SRL(struct interpreter *inter)
{
	struct opcode *op = inter->op;
	u32 rt = inter->state->native_reg_cache[op->r.rt];

	inter->state->native_reg_cache[op->r.rd] = rt >> op->r.imm;

	JUMP_NEXT(inter);
}

static void int_special_SRA(struct interpreter *inter)
{
	struct opcode *op = inter->op;
	s32 rt = inter->state->native_reg_cache[op->r.rt];

	inter->state->native_reg_cache[op->r.rd] = rt >> op->r.imm;

	JUMP_NEXT(inter);
}

static void int_special_SLLV(struct interpreter *inter)
{
	struct opcode *op = inter->op;
	u32 rs = inter->state->native_reg_cache[op->r.rs];
	u32 rt = inter->state->native_reg_cache[op->r.rt];

	inter->state->native_reg_cache[op->r.rd] = rt << (rs & 0x1f);

	JUMP_NEXT(inter);
}

static void int_special_SRLV(struct interpreter *inter)
{
	struct opcode *op = inter->op;
	u32 rs = inter->state->native_reg_cache[op->r.rs];
	u32 rt = inter->state->native_reg_cache[op->r.rt];

	inter->state->native_reg_cache[op->r.rd] = rt >> (rs & 0x1f);

	JUMP_NEXT(inter);
}

static void int_special_SRAV(struct interpreter *inter)
{
	struct opcode *op = inter->op;
	u32 rs = inter->state->native_reg_cache[op->r.rs];
	s32 rt = inter->state->native_reg_cache[op->r.rt];

	inter->state->native_reg_cache[op->r.rd] = rt >> (rs & 0x1f);

	JUMP_NEXT(inter);
}

static void int_syscall_break(struct interpreter *inter)
{

	if (inter->op->r.op == OP_SPECIAL_BREAK)
		inter->state->exit_flags |= LIGHTREC_EXIT_BREAK;
	else
		inter->state->exit_flags |= LIGHTREC_EXIT_SYSCALL;
}

static void int_special_MFHI(struct interpreter *inter)
{
	u32 *reg_cache = inter->state->native_reg_cache;
	struct opcode_r *op = &inter->op->r;

	if (likely(op->rd))
		reg_cache[op->rd] = reg_cache[REG_HI];

	JUMP_NEXT(inter);
}

static void int_special_MTHI(struct interpreter *inter)
{
	u32 *reg_cache = inter->state->native_reg_cache;

	reg_cache[REG_HI] = reg_cache[inter->op->r.rs];

	JUMP_NEXT(inter);
}

static void int_special_MFLO(struct interpreter *inter)
{
	u32 *reg_cache = inter->state->native_reg_cache;
	struct opcode_r *op = &inter->op->r;

	if (likely(op->rd))
		reg_cache[op->rd] = reg_cache[REG_LO];

	JUMP_NEXT(inter);
}

static void int_special_MTLO(struct interpreter *inter)
{
	u32 *reg_cache = inter->state->native_reg_cache;

	reg_cache[REG_LO] = reg_cache[inter->op->r.rs];

	JUMP_NEXT(inter);
}

static void int_special_MULT(struct interpreter *inter)
{
	u32 *reg_cache = inter->state->native_reg_cache;
	s32 rs = reg_cache[inter->op->r.rs];
	s32 rt = reg_cache[inter->op->r.rt];
	u64 res = (s64)rs * (s64)rt;

	reg_cache[REG_HI] = res >> 32;
	reg_cache[REG_LO] = res;

	JUMP_NEXT(inter);
}

static void int_special_MULTU(struct interpreter *inter)
{
	u32 *reg_cache = inter->state->native_reg_cache;
	u32 rs = reg_cache[inter->op->r.rs];
	u32 rt = reg_cache[inter->op->r.rt];
	u64 res = (u64)rs * (u64)rt;

	reg_cache[REG_HI] = res >> 32;
	reg_cache[REG_LO] = res;

	JUMP_NEXT(inter);
}

static void int_special_DIV(struct interpreter *inter)
{
	u32 *reg_cache = inter->state->native_reg_cache;
	s32 rs = reg_cache[inter->op->r.rs];
	s32 rt = reg_cache[inter->op->r.rt];
	u32 lo, hi;

	if (rt == 0) {
		hi = rs;
		lo = (rs < 0) * 2 - 1;
	} else {
		lo = rs / rt;
		hi = rs % rt;
	}

	reg_cache[REG_HI] = hi;
	reg_cache[REG_LO] = lo;

	JUMP_NEXT(inter);
}

static void int_special_DIVU(struct interpreter *inter)
{
	u32 *reg_cache = inter->state->native_reg_cache;
	u32 rs = reg_cache[inter->op->r.rs];
	u32 rt = reg_cache[inter->op->r.rt];
	u32 lo, hi;

	if (rt == 0) {
		hi = rs;
		lo = (u32)-1;
	} else {
		lo = rs / rt;
		hi = rs % rt;
	}

	reg_cache[REG_HI] = hi;
	reg_cache[REG_LO] = lo;

	JUMP_NEXT(inter);
}

static void int_special_ADD(struct interpreter *inter)
{
	u32 *reg_cache = inter->state->native_reg_cache;
	struct opcode_r *op = &inter->op->r;
	s32 rs = reg_cache[op->rs];
	s32 rt = reg_cache[op->rt];

	if (likely(op->rd))
		reg_cache[op->rd] = rs + rt;

	JUMP_NEXT(inter);
}

static void int_special_SUB(struct interpreter *inter)
{
	u32 *reg_cache = inter->state->native_reg_cache;
	struct opcode_r *op = &inter->op->r;
	u32 rs = reg_cache[op->rs];
	u32 rt = reg_cache[op->rt];

	if (likely(op->rd))
		reg_cache[op->rd] = rs - rt;

	JUMP_NEXT(inter);
}

static void int_special_AND(struct interpreter *inter)
{
	u32 *reg_cache = inter->state->native_reg_cache;
	struct opcode_r *op = &inter->op->r;
	u32 rs = reg_cache[op->rs];
	u32 rt = reg_cache[op->rt];

	if (likely(op->rd))
		reg_cache[op->rd] = rs & rt;

	JUMP_NEXT(inter);
}

static void int_special_OR(struct interpreter *inter)
{
	u32 *reg_cache = inter->state->native_reg_cache;
	struct opcode_r *op = &inter->op->r;
	u32 rs = reg_cache[op->rs];
	u32 rt = reg_cache[op->rt];

	if (likely(op->rd))
		reg_cache[op->rd] = rs | rt;

	JUMP_NEXT(inter);
}

static void int_special_XOR(struct interpreter *inter)
{
	u32 *reg_cache = inter->state->native_reg_cache;
	struct opcode_r *op = &inter->op->r;
	u32 rs = reg_cache[op->rs];
	u32 rt = reg_cache[op->rt];

	if (likely(op->rd))
		reg_cache[op->rd] = rs ^ rt;

	JUMP_NEXT(inter);
}

static void int_special_NOR(struct interpreter *inter)
{
	u32 *reg_cache = inter->state->native_reg_cache;
	struct opcode_r *op = &inter->op->r;
	u32 rs = reg_cache[op->rs];
	u32 rt = reg_cache[op->rt];

	if (likely(op->rd))
		reg_cache[op->rd] = ~(rs | rt);

	JUMP_NEXT(inter);
}

static void int_special_SLT(struct interpreter *inter)
{
	u32 *reg_cache = inter->state->native_reg_cache;
	struct opcode_r *op = &inter->op->r;
	s32 rs = reg_cache[op->rs];
	s32 rt = reg_cache[op->rt];

	if (likely(op->rd))
		reg_cache[op->rd] = rs < rt;

	JUMP_NEXT(inter);
}

static void int_special_SLTU(struct interpreter *inter)
{
	u32 *reg_cache = inter->state->native_reg_cache;
	struct opcode_r *op = &inter->op->r;
	u32 rs = reg_cache[op->rs];
	u32 rt = reg_cache[op->rt];

	if (likely(op->rd))
		reg_cache[op->rd] = rs < rt;

	JUMP_NEXT(inter);
}

static void int_META_UNLOAD(struct interpreter *inter)
{
	JUMP_SKIP(inter);
}

static const lightrec_int_func_t int_standard[64] = {
	[OP_SPECIAL]		= int_SPECIAL,
	[OP_REGIMM]		= int_REGIMM,
	[OP_J]			= int_J,
	[OP_JAL]		= int_JAL,
	[OP_BEQ]		= int_BEQ,
	[OP_BNE]		= int_BNE,
	[OP_BLEZ]		= int_BLEZ,
	[OP_BGTZ]		= int_BGTZ,
	[OP_ADDI]		= int_ADDI,
	[OP_ADDIU]		= int_ADDI,
	[OP_SLTI]		= int_SLTI,
	[OP_SLTIU]		= int_SLTIU,
	[OP_ANDI]		= int_ANDI,
	[OP_ORI]		= int_ORI,
	[OP_XORI]		= int_XORI,
	[OP_LUI]		= int_LUI,
	[OP_CP0]		= int_CP0,
	[OP_CP2]		= int_CP2,
	[OP_LB]			= int_load,
	[OP_LH]			= int_load,
	[OP_LWL]		= int_load,
	[OP_LW]			= int_load,
	[OP_LBU]		= int_load,
	[OP_LHU]		= int_load,
	[OP_LWR]		= int_load,
	[OP_SB]			= int_store,
	[OP_SH]			= int_store,
	[OP_SWL]		= int_store,
	[OP_SW]			= int_store,
	[OP_SWR]		= int_store,
	[OP_LWC2]		= int_LWC2,
	[OP_SWC2]		= int_store,

	[OP_META_REG_UNLOAD]	= int_META_UNLOAD,
};

static const lightrec_int_func_t int_special[64] = {
	[OP_SPECIAL_SLL]	= int_special_SLL,
	[OP_SPECIAL_SRL]	= int_special_SRL,
	[OP_SPECIAL_SRA]	= int_special_SRA,
	[OP_SPECIAL_SLLV]	= int_special_SLLV,
	[OP_SPECIAL_SRLV]	= int_special_SRLV,
	[OP_SPECIAL_SRAV]	= int_special_SRAV,
	[OP_SPECIAL_JR]		= int_special_JR,
	[OP_SPECIAL_JALR]	= int_special_JALR,
	[OP_SPECIAL_SYSCALL]	= int_syscall_break,
	[OP_SPECIAL_BREAK]	= int_syscall_break,
	[OP_SPECIAL_MFHI]	= int_special_MFHI,
	[OP_SPECIAL_MTHI]	= int_special_MTHI,
	[OP_SPECIAL_MFLO]	= int_special_MFLO,
	[OP_SPECIAL_MTLO]	= int_special_MTLO,
	[OP_SPECIAL_MULT]	= int_special_MULT,
	[OP_SPECIAL_MULTU]	= int_special_MULTU,
	[OP_SPECIAL_DIV]	= int_special_DIV,
	[OP_SPECIAL_DIVU]	= int_special_DIVU,
	[OP_SPECIAL_ADD]	= int_special_ADD,
	[OP_SPECIAL_ADDU]	= int_special_ADD,
	[OP_SPECIAL_SUB]	= int_special_SUB,
	[OP_SPECIAL_SUBU]	= int_special_SUB,
	[OP_SPECIAL_AND]	= int_special_AND,
	[OP_SPECIAL_OR]		= int_special_OR,
	[OP_SPECIAL_XOR]	= int_special_XOR,
	[OP_SPECIAL_NOR]	= int_special_NOR,
	[OP_SPECIAL_SLT]	= int_special_SLT,
	[OP_SPECIAL_SLTU]	= int_special_SLTU,
};

static const lightrec_int_func_t int_regimm[64] = {
	[OP_REGIMM_BLTZ]	= int_regimm_BLTZ,
	[OP_REGIMM_BGEZ]	= int_regimm_BGEZ,
	[OP_REGIMM_BLTZAL]	= int_regimm_BLTZAL,
	[OP_REGIMM_BGEZAL]	= int_regimm_BGEZAL,
};

static const lightrec_int_func_t int_cp0[64] = {
	[OP_CP0_MFC0]		= int_cfc,
	[OP_CP0_CFC0]		= int_cfc,
	[OP_CP0_MTC0]		= int_ctc,
	[OP_CP0_CTC0]		= int_ctc,
	[OP_CP0_RFE]		= int_cp0_RFE,
};

static const lightrec_int_func_t int_cp2_basic[64] = {
	[OP_CP2_BASIC_MFC2]	= int_cfc,
	[OP_CP2_BASIC_CFC2]	= int_cfc,
	[OP_CP2_BASIC_MTC2]	= int_ctc,
	[OP_CP2_BASIC_CTC2]	= int_ctc,
};

static void int_SPECIAL(struct interpreter *inter)
{
	lightrec_int_func_t f = int_special[inter->op->r.op];
	if (likely(f))
		EXECUTE(f, inter);

	int_unimplemented(inter);
}

static void int_REGIMM(struct interpreter *inter)
{
	lightrec_int_func_t f = int_regimm[inter->op->r.rt];
	if (likely(f))
		EXECUTE(f, inter);

	int_unimplemented(inter);
}

static void int_CP0(struct interpreter *inter)
{
	lightrec_int_func_t f = int_cp0[inter->op->r.rs];
	if (likely(f))
		EXECUTE(f, inter);
	else
		int_CP(inter);
}

static void int_CP2(struct interpreter *inter)
{
	if (inter->op->r.op == OP_CP2_BASIC) {
		lightrec_int_func_t f = int_cp2_basic[inter->op->r.rs];
		if (likely(f))
			EXECUTE(f, inter);
	}

	int_CP(inter);
}

static void lightrec_int_op(struct interpreter *inter)
{
	EXECUTE(int_standard[inter->op->i.op], inter);
}

u32 lightrec_emulate_block(struct block *block)
{
	struct interpreter inter;

	inter.block = block;
	inter.state = block->state;
	inter.op = block->opcode_list;
	inter.pc = block->pc;
	inter.cycles = 0;
	inter.delay_slot = false;

	lightrec_int_op(&inter);

	/* Add the cycles of the last branch */
	inter.cycles += lightrec_cycles_of_opcode(inter.op);

	block->state->current_cycle += inter.cycles;

	return inter.pc;
}
