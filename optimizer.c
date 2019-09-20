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
#include "memmanager.h"
#include "optimizer.h"
#include "regcache.h"

#include <errno.h>
#include <stdbool.h>
#include <stdlib.h>

struct optimizer_list {
	void (**optimizers)(struct opcode *);
	unsigned int nb_optimizers;
};

bool opcode_reads_register(union code op, u8 reg)
{
	switch (op.i.op) {
	case OP_SPECIAL:
		switch (op.r.op) {
		case OP_SPECIAL_SYSCALL:
		case OP_SPECIAL_BREAK:
			return false;
		case OP_SPECIAL_JR:
		case OP_SPECIAL_JALR:
		case OP_SPECIAL_MTHI:
		case OP_SPECIAL_MTLO:
			return op.r.rs == reg;
		case OP_SPECIAL_MFHI:
			return reg == REG_HI;
		case OP_SPECIAL_MFLO:
			return reg == REG_LO;
		case OP_SPECIAL_SLL:
		case OP_SPECIAL_SRL:
		case OP_SPECIAL_SRA:
			return op.r.rt == reg;
		default:
			return op.r.rs == reg || op.r.rt == reg;
		}
	case OP_CP0:
		switch (op.r.rs) {
		case OP_CP0_MTC0:
		case OP_CP0_CTC0:
			return op.r.rt == reg;
		default:
			return false;
		}
	case OP_CP2:
		if (op.r.op == OP_CP2_BASIC) {
			switch (op.r.rs) {
			case OP_CP2_BASIC_MTC2:
			case OP_CP2_BASIC_CTC2:
				return op.r.rt == reg;
			default:
				return false;
			}
		} else {
			return false;
		}
	case OP_J:
	case OP_JAL:
	case OP_LUI:
		return false;
	case OP_BEQ:
	case OP_BNE:
	case OP_LWL:
	case OP_LWR:
	case OP_SB:
	case OP_SH:
	case OP_SWL:
	case OP_SW:
	case OP_SWR:
		return op.i.rs == reg || op.i.rt == reg;
	default:
		return op.i.rs == reg;
	}
}

bool opcode_writes_register(union code op, u8 reg)
{
	switch (op.i.op) {
	case OP_SPECIAL:
		switch (op.r.op) {
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
			return op.r.rd == reg;
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
		return op.i.rt == reg;
	case OP_CP0:
		switch (op.r.rs) {
		case OP_CP0_MFC0:
		case OP_CP0_CFC0:
			return op.i.rt == reg;
		default:
			return false;
		}
	default:
		return false;
	}
}

/* TODO: Complete */
static bool is_nop(union code op)
{
	if (opcode_writes_register(op, 0)) {
		switch (op.i.op) {
		case OP_CP0:
			return op.r.rs != OP_CP0_MFC0;
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

	switch (op.i.op) {
	case OP_SPECIAL:
		switch (op.r.op) {
		case OP_SPECIAL_AND:
			return op.r.rd == op.r.rt && op.r.rd == op.r.rs;
		case OP_SPECIAL_ADD:
		case OP_SPECIAL_ADDU:
			return (op.r.rd == op.r.rt && op.r.rs == 0) ||
				(op.r.rd == op.r.rs && op.r.rt == 0);
		case OP_SPECIAL_SUB:
		case OP_SPECIAL_SUBU:
			return op.r.rd == op.r.rs && op.r.rt == 0;
		case OP_SPECIAL_OR:
			if (op.r.rd == op.r.rt)
				return op.r.rd == op.r.rs || op.r.rs == 0;
			else
				return (op.r.rd == op.r.rs) && op.r.rt == 0;
		case OP_SPECIAL_SLL:
		case OP_SPECIAL_SRA:
		case OP_SPECIAL_SRL:
			return op.r.imm == 0;
		default:
			return false;
		}
	case OP_ORI:
	case OP_ADDI:
	case OP_ADDIU:
		return op.i.rt == op.i.rs && op.i.imm == 0;
	case OP_BGTZ:
		return (op.i.rs == 0 || op.i.imm == 1);
	case OP_REGIMM:
		return (op.i.op == OP_REGIMM_BLTZ ||
				op.i.op == OP_REGIMM_BLTZAL) &&
			(op.i.rs == 0 || op.i.imm == 1);
	case OP_BNE:
		return (op.i.rs == op.i.rt || op.i.imm == 1);
	default:
		return false;
	}
}

static int lightrec_transform_to_nops(struct opcode *list)
{
	/* Transform all opcodes detected as useless to real NOPs
	 * (0x0: SLL r0, r0, #0) */
	for (; list; list = SLIST_NEXT(list, next)) {
		if (list->opcode != 0 && is_nop(list->c)) {
			DEBUG("Converting useless opcode 0x%08x to NOP\n",
					list->opcode);
			list->opcode = 0x0;
		}
	}

	return 0;
}

static bool has_delay_slot(union code op)
{
	switch (op.i.op) {
	case OP_SPECIAL:
		switch (op.r.op) {
		case OP_SPECIAL_JR:
		case OP_SPECIAL_JALR:
			return true;
		default:
			return false;
		}
	case OP_J:
	case OP_JAL:
	case OP_BEQ:
	case OP_BNE:
	case OP_BLEZ:
	case OP_BGTZ:
	case OP_REGIMM:
		return true;
	default:
		return false;
	}
}

static int lightrec_add_unload(struct opcode *op, u8 reg)
{
	struct opcode *meta = lightrec_malloc(MEM_FOR_IR, sizeof(*meta));

	if (!meta)
		return -ENOMEM;

	meta->i.op = OP_META_REG_UNLOAD;
	meta->i.rs = reg;
	meta->flags = LIGHTREC_SKIP_PC_UPDATE;
	meta->offset = op->offset;
	SLIST_INSERT_AFTER(op, meta, next);

	return 0;
}

static int lightrec_early_unload(struct opcode *list)
{
	u8 i;

	for (i = 1; i < 34; i++) {
		struct opcode *op, *last = NULL, *last_r = NULL, *last_w = NULL;
		unsigned int last_r_id = 0, last_w_id = 0, id = 0;
		int ret;

		for (op = list; op; last = op,
				op = SLIST_NEXT(op, next), id++) {
			if (has_delay_slot(op->c) ||
			    (last && has_delay_slot(last->c)))
				continue;

			if (opcode_reads_register(op->c, i)) {
				last_r = op;
				last_r_id = id;
			}

			if (opcode_writes_register(op->c, i)) {
				last_w = op;
				last_w_id = id;
			}
		}

		if (last_r) {
			ret = lightrec_add_unload(last_r, i);
			if (ret)
				return ret;
		}

		if (last_w && (!last_r || last_w_id > last_r_id)) {
			ret = lightrec_add_unload(last_w, i);
			if (ret)
				return ret;
		}
	}

	return 0;
}

static int (*lightrec_optimizers[])(struct opcode *) = {
	&lightrec_transform_to_nops,
	&lightrec_early_unload,
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
