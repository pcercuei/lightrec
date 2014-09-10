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
#include "disassembler.h"
#include "recompiler.h"
#include "lightrec.h"

typedef void (*lightrec_rec_func_t)(jit_state_t *, union opcode,
		const struct block *, u32);

/* Forward declarations */
static void rec_SPECIAL(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc);
static void rec_REGIMM(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc);
static void rec_CP0(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc);
static void rec_CP2(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc);
static void rec_cp2_BASIC(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc);

static lightrec_rec_func_t rec_standard[64] = {
	[OP_SPECIAL]		= rec_SPECIAL,
	[OP_REGIMM]		= rec_REGIMM,
	[OP_J]			= rec_J,
	[OP_JAL]		= rec_JAL,
	[OP_BEQ]		= rec_BEQ,
	[OP_BNE]		= rec_BNE,
	[OP_BLEZ]		= rec_BLEZ,
	[OP_BGTZ]		= rec_BGTZ,
	[OP_ADDI]		= rec_ADDI,
	[OP_ADDIU]		= rec_ADDIU,
	[OP_SLTI]		= rec_SLTI,
	[OP_SLTIU]		= rec_SLTIU,
	[OP_ANDI]		= rec_ANDI,
	[OP_ORI]		= rec_ORI,
	[OP_XORI]		= rec_XORI,
	[OP_LUI]		= rec_LUI,
	[OP_CP0]		= rec_CP0,
	[OP_CP2]		= rec_CP2,
	[OP_LB]			= rec_LB,
	[OP_LH]			= rec_LH,
	[OP_LWL]		= rec_LWL,
	[OP_LW]			= rec_LW,
	[OP_LBU]		= rec_LBU,
	[OP_LHU]		= rec_LHU,
	[OP_LWR]		= rec_LWR,
	[OP_SB]			= rec_SB,
	[OP_SH]			= rec_SH,
	[OP_SWL]		= rec_SWL,
	[OP_SW]			= rec_SW,
	[OP_SWR]		= rec_SWR,
	[OP_LWC2]		= rec_LWC2,
	[OP_SWC2]		= rec_SWC2,
	[OP_HLE]		= rec_HLE,
};

static lightrec_rec_func_t rec_special[64] = {
	[OP_SPECIAL_SLL]	= rec_special_SLL,
	[OP_SPECIAL_SRL]	= rec_special_SRL,
	[OP_SPECIAL_SRA]	= rec_special_SRA,
	[OP_SPECIAL_SLLV]	= rec_special_SLLV,
	[OP_SPECIAL_SRLV]	= rec_special_SRLV,
	[OP_SPECIAL_SRAV]	= rec_special_SRAV,
	[OP_SPECIAL_JR]		= rec_special_JR,
	[OP_SPECIAL_JALR]	= rec_special_JALR,
	[OP_SPECIAL_SYSCALL]	= rec_special_SYSCALL,
	[OP_SPECIAL_MFHI]	= rec_special_MFHI,
	[OP_SPECIAL_MTHI]	= rec_special_MTHI,
	[OP_SPECIAL_MFLO]	= rec_special_MFLO,
	[OP_SPECIAL_MTLO]	= rec_special_MTLO,
	[OP_SPECIAL_MULT]	= rec_special_MULT,
	[OP_SPECIAL_MULTU]	= rec_special_MULTU,
	[OP_SPECIAL_DIV]	= rec_special_DIV,
	[OP_SPECIAL_DIVU]	= rec_special_DIVU,
	[OP_SPECIAL_ADD]	= rec_special_ADD,
	[OP_SPECIAL_ADDU]	= rec_special_ADDU,
	[OP_SPECIAL_SUB]	= rec_special_SUB,
	[OP_SPECIAL_SUBU]	= rec_special_SUBU,
	[OP_SPECIAL_AND]	= rec_special_AND,
	[OP_SPECIAL_OR]		= rec_special_OR,
	[OP_SPECIAL_XOR]	= rec_special_XOR,
	[OP_SPECIAL_NOR]	= rec_special_NOR,
	[OP_SPECIAL_SLT]	= rec_special_SLT,
	[OP_SPECIAL_SLTU]	= rec_special_SLTU,
};

static lightrec_rec_func_t rec_regimm[64] = {
	[OP_REGIMM_BLTZ]	= rec_regimm_BLTZ,
	[OP_REGIMM_BGEZ]	= rec_regimm_BGEZ,
	[OP_REGIMM_BLTZAL]	= rec_regimm_BLTZAL,
	[OP_REGIMM_BGEZAL]	= rec_regimm_BGEZAL,
};

static lightrec_rec_func_t rec_cp0[64] = {
	[OP_CP0_MFC0]		= rec_cp0_MFC0,
	[OP_CP0_CFC0]		= rec_cp0_CFC0,
	[OP_CP0_MTC0]		= rec_cp0_MTC0,
	[OP_CP0_CTC0]		= rec_cp0_CTC0,
	[OP_CP0_RFE]		= rec_cp0_RFE,
};

static lightrec_rec_func_t rec_cp2[64] = {
	[OP_CP2_BASIC]		= rec_cp2_BASIC,
	[OP_CP2_RTPS]		= rec_cp2_RTPS,
	[OP_CP2_NCLIP]		= rec_cp2_NCLIP,
	[OP_CP2_DPCS]		= rec_cp2_DPCS,
	[OP_CP2_INTPL]		= rec_cp2_INTPL,
	[OP_CP2_MVMVA]		= rec_cp2_MVMVA,
	[OP_CP2_NCDS]		= rec_cp2_NCDS,
	[OP_CP2_CDP]		= rec_cp2_CDP,
	[OP_CP2_NCDT]		= rec_cp2_NCDT,
	[OP_CP2_NCCS]		= rec_cp2_NCCS,
	[OP_CP2_CC]		= rec_cp2_CC,
	[OP_CP2_NCS]		= rec_cp2_NCS,
	[OP_CP2_NCT]		= rec_cp2_NCT,
	[OP_CP2_SQR]		= rec_cp2_SQR,
	[OP_CP2_DCPL]		= rec_cp2_DCPL,
	[OP_CP2_DPCT]		= rec_cp2_DPCT,
	[OP_CP2_AVSZ3]		= rec_cp2_AVSZ3,
	[OP_CP2_AVSZ4]		= rec_cp2_AVSZ4,
	[OP_CP2_RTPT]		= rec_cp2_RTPT,
	[OP_CP2_GPF]		= rec_cp2_GPF,
	[OP_CP2_GPL]		= rec_cp2_GPL,
	[OP_CP2_NCCT]		= rec_cp2_NCCT,
};

static lightrec_rec_func_t rec_cp2_basic[64] = {
	[OP_CP2_BASIC_MFC2]	= rec_cp2_basic_MFC2,
	[OP_CP2_BASIC_CFC2]	= rec_cp2_basic_CFC2,
	[OP_CP2_BASIC_MTC2]	= rec_cp2_basic_MTC2,
	[OP_CP2_BASIC_CTC2]	= rec_cp2_basic_CTC2,
};

static void rec_SPECIAL(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	lightrec_rec_func_t f = rec_special[op.r.op];
	if (f)
		(*f)(_jit, op, block, pc);
	else
		emit_call_to_interpreter(_jit, op, block, pc);
}

static void rec_REGIMM(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	lightrec_rec_func_t f = rec_regimm[op.r.rt];
	if (f)
		(*f)(_jit, op, block, pc);
	else
		emit_call_to_interpreter(_jit, op, block, pc);
}

static void rec_CP0(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	lightrec_rec_func_t f = rec_cp0[op.r.rs];
	if (f)
		(*f)(_jit, op, block, pc);
	else
		emit_call_to_interpreter(_jit, op, block, pc);
}

static void rec_CP2(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	lightrec_rec_func_t f = rec_cp2[op.r.op];
	if (f)
		(*f)(_jit, op, block, pc);
	else
		emit_call_to_interpreter(_jit, op, block, pc);
}

static void rec_cp2_BASIC(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	lightrec_rec_func_t f = rec_cp2_basic[op.r.rs];
	if (f)
		(*f)(_jit, op, block, pc);
	else
		emit_call_to_interpreter(_jit, op, block, pc);
}

void lightrec_rec_opcode(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc)
{
	lightrec_rec_func_t f = rec_standard[op.i.op];
	if (f)
		(*f)(_jit, op, block, pc);
	else
		emit_call_to_interpreter(_jit, op, block, pc);
}
