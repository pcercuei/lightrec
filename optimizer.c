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

#include <stdbool.h>

/* TODO: Complete */
static bool is_nop(struct opcode *op)
{
	if (op->r.zero == 0) {
		if (op->r.rd == 0)
			return true;

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
	} else {
		switch (op->i.op) {
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
}

static void lightrec_transform_to_nops(struct opcode *list)
{
	/* Transform all opcodes detected as useless to real NOPs
	 * (0x0: SLL r0, r0, #0) */
	for (; list; list = SLIST_NEXT(list, next))
		if (is_nop(list))
			list->opcode = 0x0;
}

static void (*lightrec_optimizers[])(struct opcode *) = {
	&lightrec_transform_to_nops,
};

struct optimizer_list lightrec_optimizer_list = {
	.optimizers = lightrec_optimizers,
	.nb_optimizers = ARRAY_SIZE(lightrec_optimizers),
};
