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

#ifndef __RECOMPILER_H__
#define __RECOMPILER_H__

#include "disassembler.h"
#include "lightrec.h"

int lightrec_rec_opcode(jit_state_t *_jit, union opcode op,
		const struct block *block, u32 pc);

__weak int rec_J(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_JAL(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_BEQ(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_BNE(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_BLEZ(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_BGTZ(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_ADDI(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_ADDIU(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_SLTI(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_SLTIU(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_ANDI(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_ORI(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_XORI(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_LUI(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_LB(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_LH(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_LWL(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_LW(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_LBU(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_LHU(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_LWR(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_SB(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_SH(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_SWL(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_SW(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_SWR(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_LWC2(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_SWC2(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_HLE(jit_state_t *, union opcode,
		const struct block *, u32);

__weak int rec_special_SLL(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_special_SRL(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_special_SRA(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_special_SLLV(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_special_SRLV(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_special_SRAV(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_special_JR(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_special_JALR(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_special_SYSCALL(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_special_BREAK(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_special_MFHI(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_special_MTHI(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_special_MFLO(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_special_MTLO(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_special_MULT(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_special_MULTU(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_special_DIV(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_special_DIVU(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_special_ADD(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_special_ADDU(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_special_SUB(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_special_SUBU(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_special_AND(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_special_OR(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_special_XOR(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_special_NOR(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_special_SLT(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_special_SLTU(jit_state_t *, union opcode,
		const struct block *, u32);

__weak int rec_regimm_BLTZ(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_regimm_BGEZ(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_regimm_BLTZAL(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_regimm_BGEZAL(jit_state_t *, union opcode,
		const struct block *, u32);

__weak int rec_cp0_MFC0(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_cp0_CFC0(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_cp0_MTC0(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_cp0_CTC0(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_cp0_RFE(jit_state_t *, union opcode,
		const struct block *, u32);

__weak int rec_cp2_RTPS(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_cp2_NCLIP(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_cp2_DPCS(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_cp2_INTPL(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_cp2_MVMVA(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_cp2_NCDS(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_cp2_CDP(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_cp2_NCDT(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_cp2_NCCS(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_cp2_CC(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_cp2_NCS(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_cp2_NCT(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_cp2_SQR(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_cp2_DCPL(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_cp2_DPCT(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_cp2_AVSZ3(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_cp2_AVSZ4(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_cp2_RTPT(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_cp2_GPF(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_cp2_GPL(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_cp2_NCCT(jit_state_t *, union opcode,
		const struct block *, u32);

__weak int rec_cp2_basic_MFC2(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_cp2_basic_CFC2(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_cp2_basic_MTC2(jit_state_t *, union opcode,
		const struct block *, u32);
__weak int rec_cp2_basic_CTC2(jit_state_t *, union opcode,
		const struct block *, u32);

#endif /* __RECOMPILER_H__ */
