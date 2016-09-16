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

#include "disassembler.h"
#include "lightrec.h"
#include "optimizer.h"
#include "regcache.h"

#include <stdbool.h>

struct optimizer_list {
	void (**optimizers)(struct opcode *);
	unsigned int nb_optimizers;
};

static bool opcode_writes_register(struct opcode *op, u8 reg)
{
	switch (op->i.op) {
	case OP_SPECIAL:
		switch (op->r.op) {
		case OP_SPECIAL_JR:
		case OP_SPECIAL_JALR:
		case OP_SPECIAL_SYSCALL:
		case OP_SPECIAL_BREAK:
			return false;
		case OP_SPECIAL_MULT:
		case OP_SPECIAL_MULTU:
		case OP_SPECIAL_DIV:
		case OP_SPECIAL_DIVU:
			return reg == REG_LO || reg == REG_HI;
		case OP_SPECIAL_MTHI:
			return reg == REG_HI;
		case OP_SPECIAL_MTLO:
			return reg == REG_LO;
		default:
			return op->r.rd == reg;
		}
	case OP_ADDI:
	case OP_ADDIU:
	case OP_SLTI:
	case OP_SLTIU:
	case OP_ANDI:
	case OP_ORI:
	case OP_XORI:
	case OP_LUI:
	case OP_LB:
	case OP_LH:
	case OP_LWL:
	case OP_LW:
	case OP_LBU:
	case OP_LHU:
	case OP_LWR:
		return op->i.rt == reg;
	case OP_CP0:
		switch (op->r.rs) {
		case OP_CP0_MFC0:
		case OP_CP0_CFC0:
			return op->i.rt == reg;
		default:
			return false;
		}
	default:
		return false;
	}
}

/* TODO: Complete */
static bool is_nop(struct opcode *op)
{
	if (opcode_writes_register(op, 0)) {
		switch (op->i.op) {
		case OP_CP0:
			return op->r.rs != OP_CP0_MFC0;
		case OP_LB:
		case OP_LH:
		case OP_LWL:
		case OP_LW:
		case OP_LBU:
		case OP_LHU:
		case OP_LWR:
			return false;
		default:
			return true;
		}
	}

	switch (op->i.op) {
	case OP_SPECIAL:
		switch (op->r.op) {
		case OP_SPECIAL_AND:
			return op->r.rd == op->r.rt && op->r.rd == op->r.rs;
		case OP_SPECIAL_ADD:
		case OP_SPECIAL_ADDU:
			return (op->r.rd == op->r.rt && op->r.rs == 0) ||
				(op->r.rd == op->r.rs && op->r.rt == 0);
		case OP_SPECIAL_SUB:
		case OP_SPECIAL_SUBU:
			return op->r.rd == op->r.rs && op->r.rt == 0;
		case OP_SPECIAL_OR:
			if (op->r.rd == op->r.rt)
				return op->r.rd == op->r.rs || op->r.rs == 0;
			else
				return (op->r.rd == op->r.rs) && op->r.rt == 0;
		case OP_SPECIAL_SLL:
		case OP_SPECIAL_SRA:
		case OP_SPECIAL_SRL:
			return op->r.imm == 0;
		default:
			return false;
		}
	case OP_ORI:
	case OP_ADDI:
	case OP_ADDIU:
		return op->i.rt == op->i.rs && op->i.imm == 0;
	case OP_BGTZ:
		return (op->i.rs == 0 || op->i.imm == 1);
	case OP_REGIMM:
		return (op->i.op == OP_REGIMM_BLTZ ||
				op->i.op == OP_REGIMM_BLTZAL) &&
			(op->i.rs == 0 || op->i.imm == 1);
	case OP_BNE:
		return (op->i.rs == op->i.rt || op->i.imm == 1);
	default:
		return false;
	}
}

static int lightrec_transform_to_nops(struct opcode *list)
{
	/* Transform all opcodes detected as useless to real NOPs
	 * (0x0: SLL r0, r0, #0) */
	for (; list; list = SLIST_NEXT(list, next)) {
		if (list->opcode != 0 && is_nop(list)) {
			DEBUG("Converting useless opcode 0x%08x to NOP\n",
					list->opcode);
			list->opcode = 0x0;
		}
	}

	return 0;
}

static int (*lightrec_optimizers[])(struct opcode *) = {
	&lightrec_transform_to_nops,
};

int lightrec_optimize(struct opcode *list)
{
	unsigned int i;

	for (i = 0; i < ARRAY_SIZE(lightrec_optimizers); i++) {
		int ret = lightrec_optimizers[i](list);

		if (ret)
			return ret;
	}

	return 0;
}
