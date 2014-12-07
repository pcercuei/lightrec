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

int lightrec_rec_opcode(const struct block *block, union opcode op, u32 pc);

__weak int rec_J(const struct block *, union opcode, u32);
__weak int rec_JAL(const struct block *, union opcode, u32);
__weak int rec_BEQ(const struct block *, union opcode, u32);
__weak int rec_BNE(const struct block *, union opcode, u32);
__weak int rec_BLEZ(const struct block *, union opcode, u32);
__weak int rec_BGTZ(const struct block *, union opcode, u32);
__weak int rec_ADDI(const struct block *, union opcode, u32);
__weak int rec_ADDIU(const struct block *, union opcode, u32);
__weak int rec_SLTI(const struct block *, union opcode, u32);
__weak int rec_SLTIU(const struct block *, union opcode, u32);
__weak int rec_ANDI(const struct block *, union opcode, u32);
__weak int rec_ORI(const struct block *, union opcode, u32);
__weak int rec_XORI(const struct block *, union opcode, u32);
__weak int rec_LUI(const struct block *, union opcode, u32);
__weak int rec_LB(const struct block *, union opcode, u32);
__weak int rec_LH(const struct block *, union opcode, u32);
__weak int rec_LWL(const struct block *, union opcode, u32);
__weak int rec_LW(const struct block *, union opcode, u32);
__weak int rec_LBU(const struct block *, union opcode, u32);
__weak int rec_LHU(const struct block *, union opcode, u32);
__weak int rec_LWR(const struct block *, union opcode, u32);
__weak int rec_SB(const struct block *, union opcode, u32);
__weak int rec_SH(const struct block *, union opcode, u32);
__weak int rec_SWL(const struct block *, union opcode, u32);
__weak int rec_SW(const struct block *, union opcode, u32);
__weak int rec_SWR(const struct block *, union opcode, u32);
__weak int rec_LWC2(const struct block *, union opcode, u32);
__weak int rec_SWC2(const struct block *, union opcode, u32);
__weak int rec_HLE(const struct block *, union opcode, u32);

__weak int rec_special_SLL(const struct block *, union opcode, u32);
__weak int rec_special_SRL(const struct block *, union opcode, u32);
__weak int rec_special_SRA(const struct block *, union opcode, u32);
__weak int rec_special_SLLV(const struct block *, union opcode, u32);
__weak int rec_special_SRLV(const struct block *, union opcode, u32);
__weak int rec_special_SRAV(const struct block *, union opcode, u32);
__weak int rec_special_JR(const struct block *, union opcode, u32);
__weak int rec_special_JALR(const struct block *, union opcode, u32);
__weak int rec_special_SYSCALL(const struct block *, union opcode, u32);
__weak int rec_special_BREAK(const struct block *, union opcode, u32);
__weak int rec_special_MFHI(const struct block *, union opcode, u32);
__weak int rec_special_MTHI(const struct block *, union opcode, u32);
__weak int rec_special_MFLO(const struct block *, union opcode, u32);
__weak int rec_special_MTLO(const struct block *, union opcode, u32);
__weak int rec_special_MULT(const struct block *, union opcode, u32);
__weak int rec_special_MULTU(const struct block *, union opcode, u32);
__weak int rec_special_DIV(const struct block *, union opcode, u32);
__weak int rec_special_DIVU(const struct block *, union opcode, u32);
__weak int rec_special_ADD(const struct block *, union opcode, u32);
__weak int rec_special_ADDU(const struct block *, union opcode, u32);
__weak int rec_special_SUB(const struct block *, union opcode, u32);
__weak int rec_special_SUBU(const struct block *, union opcode, u32);
__weak int rec_special_AND(const struct block *, union opcode, u32);
__weak int rec_special_OR(const struct block *, union opcode, u32);
__weak int rec_special_XOR(const struct block *, union opcode, u32);
__weak int rec_special_NOR(const struct block *, union opcode, u32);
__weak int rec_special_SLT(const struct block *, union opcode, u32);
__weak int rec_special_SLTU(const struct block *, union opcode, u32);

__weak int rec_regimm_BLTZ(const struct block *, union opcode, u32);
__weak int rec_regimm_BGEZ(const struct block *, union opcode, u32);
__weak int rec_regimm_BLTZAL(const struct block *, union opcode, u32);
__weak int rec_regimm_BGEZAL(const struct block *, union opcode, u32);

__weak int rec_cp0_MFC0(const struct block *, union opcode, u32);
__weak int rec_cp0_CFC0(const struct block *, union opcode, u32);
__weak int rec_cp0_MTC0(const struct block *, union opcode, u32);
__weak int rec_cp0_CTC0(const struct block *, union opcode, u32);
__weak int rec_cp0_RFE(const struct block *, union opcode, u32);

__weak int rec_cp2_RTPS(const struct block *, union opcode, u32);
__weak int rec_cp2_NCLIP(const struct block *, union opcode, u32);
__weak int rec_cp2_DPCS(const struct block *, union opcode, u32);
__weak int rec_cp2_INTPL(const struct block *, union opcode, u32);
__weak int rec_cp2_MVMVA(const struct block *, union opcode, u32);
__weak int rec_cp2_NCDS(const struct block *, union opcode, u32);
__weak int rec_cp2_CDP(const struct block *, union opcode, u32);
__weak int rec_cp2_NCDT(const struct block *, union opcode, u32);
__weak int rec_cp2_NCCS(const struct block *, union opcode, u32);
__weak int rec_cp2_CC(const struct block *, union opcode, u32);
__weak int rec_cp2_NCS(const struct block *, union opcode, u32);
__weak int rec_cp2_NCT(const struct block *, union opcode, u32);
__weak int rec_cp2_SQR(const struct block *, union opcode, u32);
__weak int rec_cp2_DCPL(const struct block *, union opcode, u32);
__weak int rec_cp2_DPCT(const struct block *, union opcode, u32);
__weak int rec_cp2_AVSZ3(const struct block *, union opcode, u32);
__weak int rec_cp2_AVSZ4(const struct block *, union opcode, u32);
__weak int rec_cp2_RTPT(const struct block *, union opcode, u32);
__weak int rec_cp2_GPF(const struct block *, union opcode, u32);
__weak int rec_cp2_GPL(const struct block *, union opcode, u32);
__weak int rec_cp2_NCCT(const struct block *, union opcode, u32);

__weak int rec_cp2_basic_MFC2(const struct block *, union opcode, u32);
__weak int rec_cp2_basic_CFC2(const struct block *, union opcode, u32);
__weak int rec_cp2_basic_MTC2(const struct block *, union opcode, u32);
__weak int rec_cp2_basic_CTC2(const struct block *, union opcode, u32);

__weak int rec_meta_LB(const struct block *, union opcode, u32);
__weak int rec_meta_LH(const struct block *, union opcode, u32);
__weak int rec_meta_LW(const struct block *, union opcode, u32);
__weak int rec_meta_LBU(const struct block *, union opcode, u32);
__weak int rec_meta_LHU(const struct block *, union opcode, u32);
__weak int rec_meta_SB(const struct block *, union opcode, u32);
__weak int rec_meta_SH(const struct block *, union opcode, u32);
__weak int rec_meta_SW(const struct block *, union opcode, u32);

#endif /* __RECOMPILER_H__ */
