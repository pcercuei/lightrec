// SPDX-License-Identifier: LGPL-2.1-or-later
/*
 * Copyright (C) 2014-2021 Paul Cercueil <paul@crapouillou.net>
 */

#include "config.h"
#include "disassembler.h"
#include "lightrec.h"
#include "memmanager.h"
#include "optimizer.h"
#include "regcache.h"

#include <errno.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#define IF_OPT(opt, ptr) ((opt) ? (ptr) : NULL)

struct optimizer_list {
	void (**optimizers)(struct opcode *);
	unsigned int nb_optimizers;
};

static bool is_nop(union code op);

bool is_unconditional_jump(union code c)
{
	switch (c.i.op) {
	case OP_SPECIAL:
		return c.r.op == OP_SPECIAL_JR || c.r.op == OP_SPECIAL_JALR;
	case OP_J:
	case OP_JAL:
		return true;
	case OP_BEQ:
	case OP_BLEZ:
		return c.i.rs == c.i.rt;
	case OP_REGIMM:
		return (c.r.rt == OP_REGIMM_BGEZ ||
			c.r.rt == OP_REGIMM_BGEZAL) && c.i.rs == 0;
	default:
		return false;
	}
}

bool is_syscall(union code c)
{
	return (c.i.op == OP_SPECIAL && c.r.op == OP_SPECIAL_SYSCALL) ||
		(c.i.op == OP_CP0 && (c.r.rs == OP_CP0_MTC0 ||
					c.r.rs == OP_CP0_CTC0) &&
		 (c.r.rd == 12 || c.r.rd == 13));
}

static bool is_alu(union code op)
{
	switch (op.i.op) {
	case OP_ADDI:
	case OP_ADDIU:
	case OP_SLTI:
	case OP_SLTIU:
	case OP_ANDI:
	case OP_ORI:
	case OP_XORI:
	case OP_LUI:
		return true;
	case OP_SPECIAL:
		switch (op.r.op) {
		case OP_SPECIAL_SLL:
		case OP_SPECIAL_SRL:
		case OP_SPECIAL_SRA:
		case OP_SPECIAL_SLLV:
		case OP_SPECIAL_SRLV:
		case OP_SPECIAL_SRAV:
		case OP_SPECIAL_ADD:
		case OP_SPECIAL_ADDU:
		case OP_SPECIAL_SUB:
		case OP_SPECIAL_SUBU:
		case OP_SPECIAL_AND:
		case OP_SPECIAL_OR:
		case OP_SPECIAL_XOR:
		case OP_SPECIAL_NOR:
		case OP_SPECIAL_SLT:
		case OP_SPECIAL_SLTU:
			return true;
		default:
			return false;
		}
	default:
		return false;
	}
}

static u64 opcode_read_mask(union code op)
{
	switch (op.i.op) {
	case OP_SPECIAL:
		switch (op.r.op) {
		case OP_SPECIAL_SYSCALL:
		case OP_SPECIAL_BREAK:
			return 0;
		case OP_SPECIAL_JR:
		case OP_SPECIAL_JALR:
		case OP_SPECIAL_MTHI:
		case OP_SPECIAL_MTLO:
			return BIT(op.r.rs);
		case OP_SPECIAL_MFHI:
			return BIT(REG_HI);
		case OP_SPECIAL_MFLO:
			return BIT(REG_LO);
		case OP_SPECIAL_SLL:
		case OP_SPECIAL_SRL:
		case OP_SPECIAL_SRA:
			return BIT(op.r.rt);
		default:
			return BIT(op.r.rs) | BIT(op.r.rt);
		}
	case OP_CP0:
		switch (op.r.rs) {
		case OP_CP0_MTC0:
		case OP_CP0_CTC0:
			return BIT(op.r.rt);
		default:
			return 0;
		}
	case OP_CP2:
		if (op.r.op == OP_CP2_BASIC) {
			switch (op.r.rs) {
			case OP_CP2_BASIC_MTC2:
			case OP_CP2_BASIC_CTC2:
				return BIT(op.r.rt);
			default:
				break;
			}
		}
		return 0;
	case OP_J:
	case OP_JAL:
	case OP_LUI:
		return 0;
	case OP_BEQ:
	case OP_BNE:
	case OP_LWL:
	case OP_LWR:
	case OP_SB:
	case OP_SH:
	case OP_SWL:
	case OP_SW:
	case OP_SWR:
		return BIT(op.i.rs) | BIT(op.i.rt);
	default:
		return BIT(op.i.rs);
	}
}

static u64 opcode_write_mask(union code op)
{
	u64 flags;

	switch (op.i.op) {
	case OP_SPECIAL:
		switch (op.r.op) {
		case OP_SPECIAL_JR:
		case OP_SPECIAL_SYSCALL:
		case OP_SPECIAL_BREAK:
			return 0;
		case OP_SPECIAL_MULT:
		case OP_SPECIAL_MULTU:
		case OP_SPECIAL_DIV:
		case OP_SPECIAL_DIVU:
			if (!OPT_FLAG_MULT_DIV)
				return BIT(REG_LO) | BIT(REG_HI);

			if (op.r.rd)
				flags = BIT(op.r.rd);
			else
				flags = BIT(REG_LO);
			if (op.r.imm)
				flags |= BIT(op.r.imm);
			else
				flags |= BIT(REG_HI);
			return flags;
		case OP_SPECIAL_MTHI:
			return BIT(REG_HI);
		case OP_SPECIAL_MTLO:
			return BIT(REG_LO);
		default:
			return BIT(op.r.rd);
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
		return BIT(op.i.rt);
	case OP_JAL:
		return BIT(31);
	case OP_CP0:
		switch (op.r.rs) {
		case OP_CP0_MFC0:
		case OP_CP0_CFC0:
			return BIT(op.i.rt);
		default:
			return 0;
		}
	case OP_CP2:
		if (op.r.op == OP_CP2_BASIC) {
			switch (op.r.rs) {
			case OP_CP2_BASIC_MFC2:
			case OP_CP2_BASIC_CFC2:
				return BIT(op.i.rt);
			default:
				break;
			}
		}
		return 0;
	case OP_REGIMM:
		switch (op.r.rt) {
		case OP_REGIMM_BLTZAL:
		case OP_REGIMM_BGEZAL:
			return BIT(31);
		default:
			return 0;
		}
	case OP_META_MOV:
		return BIT(op.r.rd);
	default:
		return 0;
	}
}

bool opcode_reads_register(union code op, u8 reg)
{
	return opcode_read_mask(op) & BIT(reg);
}

bool opcode_writes_register(union code op, u8 reg)
{
	return opcode_write_mask(op) & BIT(reg);
}

static int find_prev_writer(const struct opcode *list, unsigned int offset, u8 reg)
{
	union code c;
	unsigned int i;

	if (list[offset].flags & LIGHTREC_SYNC)
		return -1;

	for (i = offset; i > 0; i--) {
		c = list[i - 1].c;

		if (opcode_writes_register(c, reg)) {
			if (i > 1 && has_delay_slot(list[i - 2].c))
				break;

			return i - 1;
		}

		if ((list[i - 1].flags & LIGHTREC_SYNC) ||
		    has_delay_slot(c) ||
		    opcode_reads_register(c, reg))
			break;
	}

	return -1;
}

static int find_next_reader(const struct opcode *list, unsigned int offset, u8 reg)
{
	unsigned int i;
	union code c;

	if (list[offset].flags & LIGHTREC_SYNC)
		return -1;

	for (i = offset; ; i++) {
		c = list[i].c;

		if (opcode_reads_register(c, reg)) {
			if (i > 0 && has_delay_slot(list[i - 1].c))
				break;

			return i;
		}

		if ((list[i].flags & LIGHTREC_SYNC) ||
		    has_delay_slot(c) || opcode_writes_register(c, reg))
			break;
	}

	return -1;
}

static bool reg_is_dead(const struct opcode *list, unsigned int offset, u8 reg)
{
	unsigned int i;

	if (list[offset].flags & LIGHTREC_SYNC)
		return false;

	for (i = offset + 1; ; i++) {
		if (opcode_reads_register(list[i].c, reg))
			return false;

		if (opcode_writes_register(list[i].c, reg))
			return true;

		if (has_delay_slot(list[i].c)) {
			if (list[i].flags & LIGHTREC_NO_DS)
				return false;

			return opcode_writes_register(list[i + 1].c, reg);
		}
	}
}

static bool reg_is_read(const struct opcode *list,
			unsigned int a, unsigned int b, u8 reg)
{
	/* Return true if reg is read in one of the opcodes of the interval
	 * [a, b[ */
	for (; a < b; a++) {
		if (!is_nop(list[a].c) && opcode_reads_register(list[a].c, reg))
			return true;
	}

	return false;
}

static bool reg_is_written(const struct opcode *list,
			   unsigned int a, unsigned int b, u8 reg)
{
	/* Return true if reg is written in one of the opcodes of the interval
	 * [a, b[ */

	for (; a < b; a++) {
		if (!is_nop(list[a].c) && opcode_writes_register(list[a].c, reg))
			return true;
	}

	return false;
}

static bool reg_is_read_or_written(const struct opcode *list,
				   unsigned int a, unsigned int b, u8 reg)
{
	return reg_is_read(list, a, b, reg) || reg_is_written(list, a, b, reg);
}

static bool opcode_is_load(union code op)
{
	switch (op.i.op) {
	case OP_LB:
	case OP_LH:
	case OP_LWL:
	case OP_LW:
	case OP_LBU:
	case OP_LHU:
	case OP_LWR:
	case OP_LWC2:
		return true;
	default:
		return false;
	}
}

static bool opcode_is_store(union code op)
{
	switch (op.i.op) {
	case OP_SB:
	case OP_SH:
	case OP_SW:
	case OP_SWL:
	case OP_SWR:
	case OP_SWC2:
		return true;
	default:
		return false;
	}
}

bool opcode_is_io(union code op)
{
	return opcode_is_load(op) || opcode_is_store(op);
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
			return op.r.rd == op.r.rt && op.r.imm == 0;
		case OP_SPECIAL_MFHI:
		case OP_SPECIAL_MFLO:
			return op.r.rd == 0;
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

bool load_in_delay_slot(union code op)
{
	switch (op.i.op) {
	case OP_CP0:
		switch (op.r.rs) {
		case OP_CP0_MFC0:
		case OP_CP0_CFC0:
			return true;
		default:
			break;
		}

		break;
	case OP_CP2:
		if (op.r.op == OP_CP2_BASIC) {
			switch (op.r.rs) {
			case OP_CP2_BASIC_MFC2:
			case OP_CP2_BASIC_CFC2:
				return true;
			default:
				break;
			}
		}

		break;
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

static u32 lightrec_propagate_consts(const struct opcode *op, u32 known, u32 *v)
{
	union code c = op->c;

	if (op->flags & LIGHTREC_SYNC)
		return 0;

	switch (c.i.op) {
	case OP_SPECIAL:
		switch (c.r.op) {
		case OP_SPECIAL_SLL:
			if (known & BIT(c.r.rt)) {
				known |= BIT(c.r.rd);
				v[c.r.rd] = v[c.r.rt] << c.r.imm;
			} else {
				known &= ~BIT(c.r.rd);
			}
			break;
		case OP_SPECIAL_SRL:
			if (known & BIT(c.r.rt)) {
				known |= BIT(c.r.rd);
				v[c.r.rd] = v[c.r.rt] >> c.r.imm;
			} else {
				known &= ~BIT(c.r.rd);
			}
			break;
		case OP_SPECIAL_SRA:
			if (known & BIT(c.r.rt)) {
				known |= BIT(c.r.rd);
				v[c.r.rd] = (s32)v[c.r.rt] >> c.r.imm;
			} else {
				known &= ~BIT(c.r.rd);
			}
			break;
		case OP_SPECIAL_SLLV:
			if (known & BIT(c.r.rt) && known & BIT(c.r.rs)) {
				known |= BIT(c.r.rd);
				v[c.r.rd] = v[c.r.rt] << (v[c.r.rs] & 0x1f);
			} else {
				known &= ~BIT(c.r.rd);
			}
			break;
		case OP_SPECIAL_SRLV:
			if (known & BIT(c.r.rt) && known & BIT(c.r.rs)) {
				known |= BIT(c.r.rd);
				v[c.r.rd] = v[c.r.rt] >> (v[c.r.rs] & 0x1f);
			} else {
				known &= ~BIT(c.r.rd);
			}
			break;
		case OP_SPECIAL_SRAV:
			if (known & BIT(c.r.rt) && known & BIT(c.r.rs)) {
				known |= BIT(c.r.rd);
				v[c.r.rd] = (s32)v[c.r.rt]
					  >> (v[c.r.rs] & 0x1f);
			} else {
				known &= ~BIT(c.r.rd);
			}
			break;
		case OP_SPECIAL_ADD:
		case OP_SPECIAL_ADDU:
			if (known & BIT(c.r.rt) && known & BIT(c.r.rs)) {
				known |= BIT(c.r.rd);
				v[c.r.rd] = (s32)v[c.r.rt] + (s32)v[c.r.rs];
			} else {
				known &= ~BIT(c.r.rd);
			}
			break;
		case OP_SPECIAL_SUB:
		case OP_SPECIAL_SUBU:
			if (known & BIT(c.r.rt) && known & BIT(c.r.rs)) {
				known |= BIT(c.r.rd);
				v[c.r.rd] = v[c.r.rt] - v[c.r.rs];
			} else {
				known &= ~BIT(c.r.rd);
			}
			break;
		case OP_SPECIAL_AND:
			if (known & BIT(c.r.rt) && known & BIT(c.r.rs)) {
				known |= BIT(c.r.rd);
				v[c.r.rd] = v[c.r.rt] & v[c.r.rs];
			} else {
				known &= ~BIT(c.r.rd);
			}
			break;
		case OP_SPECIAL_OR:
			if (known & BIT(c.r.rt) && known & BIT(c.r.rs)) {
				known |= BIT(c.r.rd);
				v[c.r.rd] = v[c.r.rt] | v[c.r.rs];
			} else {
				known &= ~BIT(c.r.rd);
			}
			break;
		case OP_SPECIAL_XOR:
			if (known & BIT(c.r.rt) && known & BIT(c.r.rs)) {
				known |= BIT(c.r.rd);
				v[c.r.rd] = v[c.r.rt] ^ v[c.r.rs];
			} else {
				known &= ~BIT(c.r.rd);
			}
			break;
		case OP_SPECIAL_NOR:
			if (known & BIT(c.r.rt) && known & BIT(c.r.rs)) {
				known |= BIT(c.r.rd);
				v[c.r.rd] = ~(v[c.r.rt] | v[c.r.rs]);
			} else {
				known &= ~BIT(c.r.rd);
			}
			break;
		case OP_SPECIAL_SLT:
			if (known & BIT(c.r.rt) && known & BIT(c.r.rs)) {
				known |= BIT(c.r.rd);
				v[c.r.rd] = (s32)v[c.r.rs] < (s32)v[c.r.rt];
			} else {
				known &= ~BIT(c.r.rd);
			}
			break;
		case OP_SPECIAL_SLTU:
			if (known & BIT(c.r.rt) && known & BIT(c.r.rs)) {
				known |= BIT(c.r.rd);
				v[c.r.rd] = v[c.r.rs] < v[c.r.rt];
			} else {
				known &= ~BIT(c.r.rd);
			}
			break;
		default:
			break;
		}
		break;
	case OP_REGIMM:
		break;
	case OP_ADDI:
	case OP_ADDIU:
		if (known & BIT(c.i.rs)) {
			known |= BIT(c.i.rt);
			v[c.i.rt] = v[c.i.rs] + (s32)(s16)c.i.imm;
		} else {
			known &= ~BIT(c.i.rt);
		}
		break;
	case OP_SLTI:
		if (known & BIT(c.i.rs)) {
			known |= BIT(c.i.rt);
			v[c.i.rt] = (s32)v[c.i.rs] < (s32)(s16)c.i.imm;
		} else {
			known &= ~BIT(c.i.rt);
		}
		break;
	case OP_SLTIU:
		if (known & BIT(c.i.rs)) {
			known |= BIT(c.i.rt);
			v[c.i.rt] = v[c.i.rs] < (u32)(s32)(s16)c.i.imm;
		} else {
			known &= ~BIT(c.i.rt);
		}
		break;
	case OP_ANDI:
		if (known & BIT(c.i.rs)) {
			known |= BIT(c.i.rt);
			v[c.i.rt] = v[c.i.rs] & c.i.imm;
		} else {
			known &= ~BIT(c.i.rt);
		}
		break;
	case OP_ORI:
		if (known & BIT(c.i.rs)) {
			known |= BIT(c.i.rt);
			v[c.i.rt] = v[c.i.rs] | c.i.imm;
		} else {
			known &= ~BIT(c.i.rt);
		}
		break;
	case OP_XORI:
		if (known & BIT(c.i.rs)) {
			known |= BIT(c.i.rt);
			v[c.i.rt] = v[c.i.rs] ^ c.i.imm;
		} else {
			known &= ~BIT(c.i.rt);
		}
		break;
	case OP_LUI:
		known |= BIT(c.i.rt);
		v[c.i.rt] = c.i.imm << 16;
		break;
	case OP_CP0:
		switch (c.r.rs) {
		case OP_CP0_MFC0:
		case OP_CP0_CFC0:
			known &= ~BIT(c.r.rt);
			break;
		}
		break;
	case OP_CP2:
		if (c.r.op == OP_CP2_BASIC) {
			switch (c.r.rs) {
			case OP_CP2_BASIC_MFC2:
			case OP_CP2_BASIC_CFC2:
				known &= ~BIT(c.r.rt);
				break;
			}
		}
		break;
	case OP_LB:
	case OP_LH:
	case OP_LWL:
	case OP_LW:
	case OP_LBU:
	case OP_LHU:
	case OP_LWR:
	case OP_LWC2:
		known &= ~BIT(c.i.rt);
		break;
	case OP_META_MOV:
		if (known & BIT(c.r.rs)) {
			known |= BIT(c.r.rd);
			v[c.r.rd] = v[c.r.rs];
		} else {
			known &= ~BIT(c.r.rd);
		}
		break;
	default:
		break;
	}

	return known;
}

static void lightrec_optimize_sll_sra(struct opcode *list, unsigned int offset)
{
	struct opcode *prev, *prev2 = NULL, *curr = &list[offset];
	struct opcode *to_change, *to_nop;
	int idx, idx2;

	if (curr->r.imm != 24 && curr->r.imm != 16)
		return;

	idx = find_prev_writer(list, offset, curr->r.rt);
	if (idx < 0)
		return;

	prev = &list[idx];

	if (prev->i.op != OP_SPECIAL || prev->r.op != OP_SPECIAL_SLL ||
	    prev->r.imm != curr->r.imm || prev->r.rd != curr->r.rt)
		return;

	if (prev->r.rd != prev->r.rt && curr->r.rd != curr->r.rt) {
		/* sll rY, rX, 16
		 * ...
		 * srl rZ, rY, 16 */

		if (!reg_is_dead(list, offset, curr->r.rt) ||
		    reg_is_read_or_written(list, idx, offset, curr->r.rd))
			return;

		/* If rY is dead after the SRL, and rZ is not used after the SLL,
		 * we can change rY to rZ */

		pr_debug("Detected SLL/SRA with middle temp register\n");
		prev->r.rd = curr->r.rd;
		curr->r.rt = prev->r.rd;
	}

	/* We got a SLL/SRA combo. If imm #16, that's a cast to u16.
	 * If imm #24 that's a cast to u8.
	 *
	 * First of all, make sure that the target register of the SLL is not
	 * read before the SRA. */

	if (prev->r.rd == prev->r.rt) {
		/* sll rX, rX, 16
		 * ...
		 * srl rY, rX, 16 */
		to_change = curr;
		to_nop = prev;

		/* rX is used after the SRA - we cannot convert it. */
		if (prev->r.rd != curr->r.rd && !reg_is_dead(list, offset, prev->r.rd))
			return;
	} else {
		/* sll rY, rX, 16
		 * ...
		 * srl rY, rY, 16 */
		to_change = prev;
		to_nop = curr;
	}

	idx2 = find_prev_writer(list, idx, prev->r.rt);
	if (idx2 >= 0) {
		/* Note that PSX games sometimes do casts after
		 * a LHU or LBU; in this case we can change the
		 * load opcode to a LH or LB, and the cast can
		 * be changed to a MOV or a simple NOP. */

		prev2 = &list[idx2];

		if (curr->r.rd != prev2->i.rt &&
		    !reg_is_dead(list, offset, prev2->i.rt))
			prev2 = NULL;
		else if (curr->r.imm == 16 && prev2->i.op == OP_LHU)
			prev2->i.op = OP_LH;
		else if (curr->r.imm == 24 && prev2->i.op == OP_LBU)
			prev2->i.op = OP_LB;
		else
			prev2 = NULL;

		if (prev2) {
			if (curr->r.rd == prev2->i.rt) {
				to_change->opcode = 0;
			} else if (reg_is_dead(list, offset, prev2->i.rt) &&
				   !reg_is_read_or_written(list, idx2 + 1, offset, curr->r.rd)) {
				/* The target register of the SRA is dead after the
				 * LBU/LHU; we can change the target register of the
				 * LBU/LHU to the one of the SRA. */
				prev2->i.rt = curr->r.rd;
				to_change->opcode = 0;
			} else {
				to_change->i.op = OP_META_MOV;
				to_change->r.rd = curr->r.rd;
				to_change->r.rs = prev2->i.rt;
			}

			if (to_nop->r.imm == 24)
				pr_debug("Convert LBU+SLL+SRA to LB\n");
			else
				pr_debug("Convert LHU+SLL+SRA to LH\n");
		}
	}

	if (!prev2) {
		pr_debug("Convert SLL/SRA #%u to EXT%c\n",
			 prev->r.imm,
			 prev->r.imm == 24 ? 'C' : 'S');

		if (to_change == prev) {
			to_change->i.rs = prev->r.rt;
			to_change->i.rt = curr->r.rd;
		} else {
			to_change->i.rt = curr->r.rd;
			to_change->i.rs = prev->r.rt;
		}

		if (to_nop->r.imm == 24)
			to_change->i.op = OP_META_EXTC;
		else
			to_change->i.op = OP_META_EXTS;
	}

	to_nop->opcode = 0;
}

static int lightrec_transform_ops(struct lightrec_state *state, struct block *block)
{
	struct opcode *list = block->opcode_list;
	struct opcode *op;
	u32 known = BIT(0);
	u32 values[32] = { 0 };
	unsigned int i;
	int reader;

	for (i = 0; i < block->nb_ops; i++) {
		op = &list[i];

		/* Transform all opcodes detected as useless to real NOPs
		 * (0x0: SLL r0, r0, #0) */
		if (op->opcode != 0 && is_nop(op->c)) {
			pr_debug("Converting useless opcode 0x%08x to NOP\n",
					op->opcode);
			op->opcode = 0x0;
		}

		if (!op->opcode)
			continue;

		/* Register $zero is always, well, zero */
		known |= BIT(0);
		values[0] = 0;

		switch (op->i.op) {
		case OP_BEQ:
			if (op->i.rs == op->i.rt) {
				op->i.rs = 0;
				op->i.rt = 0;
			} else if (op->i.rs == 0) {
				op->i.rs = op->i.rt;
				op->i.rt = 0;
			}
			break;

		case OP_BNE:
			if (op->i.rs == 0) {
				op->i.rs = op->i.rt;
				op->i.rt = 0;
			}
			break;

		case OP_LUI:
			if (!(op->flags & LIGHTREC_SYNC) &&
			    (known & BIT(op->i.rt)) &&
			    values[op->i.rt] == op->i.imm << 16) {
				pr_debug("Converting duplicated LUI to NOP\n");
				op->opcode = 0x0;
			}

			if (op->i.imm != 0 || op->i.rt == 0)
				break;

			reader = find_next_reader(list, i + 1, op->i.rt);
			if (reader > 0 &&
			    (opcode_writes_register(list[reader].c, op->i.rt) ||
			     reg_is_dead(list, reader, op->i.rt))) {

				pr_debug("Removing useless LUI 0x0\n");

				if (list[reader].i.rs == op->i.rt)
					list[reader].i.rs = 0;
				if (list[reader].i.op == OP_SPECIAL &&
				    list[reader].i.rt == op->i.rt)
					list[reader].i.rt = 0;
				op->opcode = 0x0;
			}
			break;

		/* Transform ORI/ADDI/ADDIU with imm #0 or ORR/ADD/ADDU/SUB/SUBU
		 * with register $zero to the MOV meta-opcode */
		case OP_ORI:
		case OP_ADDI:
		case OP_ADDIU:
			if (op->i.imm == 0) {
				pr_debug("Convert ORI/ADDI/ADDIU #0 to MOV\n");
				op->i.op = OP_META_MOV;
				op->r.rd = op->i.rt;
			}
			break;
		case OP_SPECIAL:
			switch (op->r.op) {
			case OP_SPECIAL_SRA:
				if (op->r.imm == 0) {
					pr_debug("Convert SRA #0 to MOV\n");
					op->i.op = OP_META_MOV;
					op->r.rs = op->r.rt;
					break;
				}

				lightrec_optimize_sll_sra(block->opcode_list, i);
				break;
			case OP_SPECIAL_SLL:
			case OP_SPECIAL_SRL:
				if (op->r.imm == 0) {
					pr_debug("Convert SLL/SRL #0 to MOV\n");
					op->i.op = OP_META_MOV;
					op->r.rs = op->r.rt;
				}
				break;
			case OP_SPECIAL_OR:
			case OP_SPECIAL_ADD:
			case OP_SPECIAL_ADDU:
				if (op->r.rs == 0) {
					pr_debug("Convert OR/ADD $zero to MOV\n");
					op->i.op = OP_META_MOV;
					op->r.rs = op->r.rt;
				}
			case OP_SPECIAL_SUB: /* fall-through */
			case OP_SPECIAL_SUBU:
				if (op->r.rt == 0) {
					pr_debug("Convert OR/ADD/SUB $zero to MOV\n");
					op->i.op = OP_META_MOV;
				}
			default: /* fall-through */
				break;
			}
		default: /* fall-through */
			break;
		}

		known = lightrec_propagate_consts(op, known, values);
	}

	return 0;
}

static bool lightrec_can_switch_delay_slot(union code op, union code next_op)
{
	switch (op.i.op) {
	case OP_SPECIAL:
		switch (op.r.op) {
		case OP_SPECIAL_JALR:
			if (opcode_reads_register(next_op, op.r.rd) ||
			    opcode_writes_register(next_op, op.r.rd))
				return false;
		case OP_SPECIAL_JR: /* fall-through */
			if (opcode_writes_register(next_op, op.r.rs))
				return false;
		default: /* fall-through */
			break;
		}
	case OP_J: /* fall-through */
		break;
	case OP_JAL:
		if (opcode_reads_register(next_op, 31) ||
		    opcode_writes_register(next_op, 31))
			return false;;

		break;
	case OP_BEQ:
	case OP_BNE:
		if (op.i.rt && opcode_writes_register(next_op, op.i.rt))
			return false;
	case OP_BLEZ: /* fall-through */
	case OP_BGTZ:
		if (op.i.rs && opcode_writes_register(next_op, op.i.rs))
			return false;
		break;
	case OP_REGIMM:
		switch (op.r.rt) {
		case OP_REGIMM_BLTZAL:
		case OP_REGIMM_BGEZAL:
			if (opcode_reads_register(next_op, 31) ||
			    opcode_writes_register(next_op, 31))
				return false;
		case OP_REGIMM_BLTZ: /* fall-through */
		case OP_REGIMM_BGEZ:
			if (op.i.rs && opcode_writes_register(next_op, op.i.rs))
				return false;
			break;
		}
	default: /* fall-through */
		break;
	}

	return true;
}

static int lightrec_can_conditional_move(const struct opcode *list,
					 unsigned int start, unsigned int end)
{
	for (; start < end; start++)
		if (!is_alu(list[start].c) || (list[start].flags & LIGHTREC_SYNC))
			return false;

	return true;
}

static void lightrec_clear_sync(struct lightrec_state *state, struct block *block, u32 offset)
{
	struct opcode *list = block->opcode_list;
	unsigned int i;
	union code op;

	for (i = 0; i < block->nb_ops - 1; i++) {
		op = list[i].c;

		switch (op.i.op) {
		case OP_BEQ:
		case OP_BNE:
		case OP_BLEZ:
		case OP_BGTZ:
		case OP_REGIMM:
			break;
		default:
			continue;
		}

		/* If we find any local branch pointing to this offset, return early. */
		if (i + 1 + (s16)op.i.imm == offset)
			return;
	}

	/* We found no branch pointing to this offset - clear the SYNC flag */
	pr_debug("Clear sync flag at offset %u\n", offset);
	list[offset].flags &= ~LIGHTREC_SYNC;
}

static int lightrec_conditional_moves(struct lightrec_state *state, struct block *block)
{
	struct opcode *list = block->opcode_list;
	unsigned int i, idx_in, idx_out;
	union code op;
	u32 offset;
	bool is_local, has_sync;

	for (i = 0; i < block->nb_ops - 1; i++) {
		op = list[i].c;

		switch (op.i.op) {
		case OP_BEQ:
		case OP_BNE:
			/* Verify that it's a forward BEQZ/BNEZ, and that all
			 * instructions in-between the branch instruction and
			 * the target are ALU opcodes. */
			offset = i + 1 + (s16)op.i.imm;
			if (offset > i && offset < block->nb_ops &&
			    (op.i.rs == 0 || op.i.rt == 0) &&
			    lightrec_can_conditional_move(list, i + 1, offset))
				break;
		default: /* fall-through */
			continue;
		}

		if (!lightrec_can_switch_delay_slot(op, list[i + 1].c)) {
			pr_debug("Conditional move at offset %u cannot be converted.\n", i);
			continue;
		}

		pr_debug("Conditional move at offset %u\n", i);

		is_local = list[i].flags & LIGHTREC_LOCAL_BRANCH;
		has_sync = list[i].flags & LIGHTREC_SYNC;

		/* Remove the branch, and compact the ALU opcodes. */
		for (idx_in = i + 1, idx_out = i; idx_in < offset; idx_in++) {
			if (is_nop(list[idx_in].c))
				continue;

			list[idx_out] = list[idx_in];
			if (idx_in > i + 1)
				list[idx_out].flags |= LIGHTREC_REG_SHADOW;
			if (idx_out == i && has_sync)
				list[idx_out].flags |= LIGHTREC_SYNC;

			idx_out++;
		}

		/* Add COMMIT opcode */
		list[idx_out].i.op = OP_META_COMMIT;
		list[idx_out].r.rs = op.i.rs ?: op.i.rt;
		list[idx_out].r.rd = op.i.op == OP_BNE;
		list[idx_out].r.imm = offset - i - 2;
		list[idx_out].flags = 0;
		idx_out++;

		/* Zero the rest of the space */
		for (; idx_out < offset; idx_out++) {
			list[idx_out].c.opcode = 0;
			list[idx_out].flags = 0;
		}

		if (is_local)
			lightrec_clear_sync(state, block, (u32) offset);
	}

	return 0;
}

static int lightrec_switch_delay_slots(struct lightrec_state *state, struct block *block)
{
	struct opcode *list, *next = &block->opcode_list[0];
	unsigned int i;
	union code op, next_op;
	u8 flags;

	for (i = 0; i < block->nb_ops - 1; i++) {
		list = next;
		next = &block->opcode_list[i + 1];
		next_op = next->c;
		op = list->c;

		if (!has_delay_slot(op) ||
		    list->flags & (LIGHTREC_NO_DS | LIGHTREC_EMULATE_BRANCH) ||
		    op.opcode == 0 || next_op.opcode == 0)
			continue;

		if (i && has_delay_slot(block->opcode_list[i - 1].c) &&
		    !(block->opcode_list[i - 1].flags & LIGHTREC_NO_DS))
			continue;

		if ((list->flags & LIGHTREC_SYNC) ||
		    (next->flags & LIGHTREC_SYNC))
			continue;

		if (!lightrec_can_switch_delay_slot(list->c, next_op))
			continue;

		pr_debug("Swap branch and delay slot opcodes "
			 "at offsets 0x%x / 0x%x\n",
			 i << 2, (i + 1) << 2);

		flags = next->flags;
		list->c = next_op;
		next->c = op;
		next->flags = list->flags | LIGHTREC_NO_DS;
		list->flags = flags | LIGHTREC_NO_DS;
	}

	return 0;
}

static int shrink_opcode_list(struct lightrec_state *state, struct block *block, u16 new_size)
{
	struct opcode *list;

	if (new_size >= block->nb_ops) {
		pr_err("Invalid shrink size (%u vs %u)\n",
		       new_size, block->nb_ops);
		return -EINVAL;
	}


	list = lightrec_malloc(state, MEM_FOR_IR,
			       sizeof(*list) * new_size);
	if (!list) {
		pr_err("Unable to allocate memory\n");
		return -ENOMEM;
	}

	memcpy(list, block->opcode_list, sizeof(*list) * new_size);

	lightrec_free_opcode_list(state, block);
	block->opcode_list = list;
	block->nb_ops = new_size;

	pr_debug("Shrunk opcode list of block PC 0x%08x to %u opcodes\n",
		 block->pc, new_size);

	return 0;
}

static int lightrec_detect_impossible_branches(struct lightrec_state *state,
					       struct block *block)
{
	struct opcode *op, *next = &block->opcode_list[0];
	unsigned int i;
	int ret = 0;

	for (i = 0; i < block->nb_ops - 1; i++) {
		op = next;
		next = &block->opcode_list[i + 1];

		if (!has_delay_slot(op->c) ||
		    (!load_in_delay_slot(next->c) &&
		     !has_delay_slot(next->c) &&
		     !(next->i.op == OP_CP0 && next->r.rs == OP_CP0_RFE)))
			continue;

		if (op->c.opcode == next->c.opcode) {
			/* The delay slot is the exact same opcode as the branch
			 * opcode: this is effectively a NOP */
			next->c.opcode = 0;
			continue;
		}

		op->flags |= LIGHTREC_EMULATE_BRANCH;

		if (op == block->opcode_list) {
			pr_debug("First opcode of block PC 0x%08x is an impossible branch\n",
				 block->pc);

			/* If the first opcode is an 'impossible' branch, we
			 * only keep the first two opcodes of the block (the
			 * branch itself + its delay slot) */
			if (block->nb_ops > 2)
				ret = shrink_opcode_list(state, block, 2);
			break;
		}
	}

	return ret;
}

static int lightrec_local_branches(struct lightrec_state *state, struct block *block)
{
	struct opcode *list;
	unsigned int i;
	s32 offset;

	for (i = 0; i < block->nb_ops; i++) {
		list = &block->opcode_list[i];

		if (should_emulate(list))
			continue;

		switch (list->i.op) {
		case OP_BEQ:
		case OP_BNE:
		case OP_BLEZ:
		case OP_BGTZ:
		case OP_REGIMM:
			offset = i + 1 + (s16)list->i.imm;
			if (offset >= 0 && offset < block->nb_ops)
				break;
		default: /* fall-through */
			continue;
		}

		pr_debug("Found local branch to offset 0x%x\n", offset << 2);

		if (should_emulate(&block->opcode_list[offset])) {
			pr_debug("Branch target must be emulated - skip\n");
			continue;
		}

		if (offset && has_delay_slot(block->opcode_list[offset - 1].c)) {
			pr_debug("Branch target is a delay slot - skip\n");
			continue;
		}

		pr_debug("Adding sync at offset 0x%x\n", offset << 2);

		block->opcode_list[offset].flags |= LIGHTREC_SYNC;
		list->flags |= LIGHTREC_LOCAL_BRANCH;
	}

	return 0;
}

bool has_delay_slot(union code op)
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

bool should_emulate(const struct opcode *list)
{
	return has_delay_slot(list->c) &&
		(list->flags & LIGHTREC_EMULATE_BRANCH);
}

static void lightrec_add_unload(struct opcode *op, u8 reg)
{
	if (op->i.op == OP_SPECIAL && reg == op->r.rd)
		op->flags |= LIGHTREC_UNLOAD_RD;

	if (op->i.rs == reg)
		op->flags |= LIGHTREC_UNLOAD_RS;
	if (op->i.rt == reg)
		op->flags |= LIGHTREC_UNLOAD_RT;
}

static int lightrec_early_unload(struct lightrec_state *state, struct block *block)
{
	unsigned int i, offset;
	struct opcode *op;
	u32 shadows = 0;
	u8 reg;

	for (reg = 1; reg < 34; reg++) {
		int last_r_id = -1, last_w_id = -1;

		for (i = 0; i < block->nb_ops; i++) {
			union code c = block->opcode_list[i].c;

			if (OPT_CONDITIONAL_MOVES && c.i.op == OP_META_COMMIT) {
				if (shadows & BIT(reg))
					last_w_id = i;
				shadows = 0;
			} else if (opcode_reads_register(c, reg)) {
				last_r_id = i;
			} else if (opcode_writes_register(c, reg)) {
				if (OPT_CONDITIONAL_MOVES && is_alu(c) &&
				    (block->opcode_list[i].flags & LIGHTREC_REG_SHADOW))
					shadows |= BIT(reg);
				else
					last_w_id = i;
			}
		}

		if (last_w_id > last_r_id)
			offset = (unsigned int)last_w_id;
		else if (last_r_id >= 0)
			offset = (unsigned int)last_r_id;
		else
			continue;

		op = &block->opcode_list[offset];

		if (has_delay_slot(op->c) && (op->flags & LIGHTREC_NO_DS))
			offset++;

		if (offset == block->nb_ops)
			continue;

		lightrec_add_unload(&block->opcode_list[offset], reg);
	}

	return 0;
}

static int lightrec_flag_io(struct lightrec_state *state, struct block *block)
{
	const struct lightrec_mem_map *map;
	struct opcode *list;
	u32 known = BIT(0);
	u32 values[32] = { 0 };
	unsigned int i;
	u32 val;

	for (i = 0; i < block->nb_ops; i++) {
		list = &block->opcode_list[i];

		/* Register $zero is always, well, zero */
		known |= BIT(0);
		values[0] = 0;

		switch (list->i.op) {
		case OP_SB:
		case OP_SH:
		case OP_SW:
			if (OPT_FLAG_STORES) {
				/* Mark all store operations that target $sp or $gp
				 * as not requiring code invalidation. This is based
				 * on the heuristic that stores using one of these
				 * registers as address will never hit a code page. */
				if (list->i.rs >= 28 && list->i.rs <= 29 &&
				    !state->maps[PSX_MAP_KERNEL_USER_RAM].ops) {
					pr_debug("Flaging opcode 0x%08x as not "
						 "requiring invalidation\n",
						 list->opcode);
					list->flags |= LIGHTREC_NO_INVALIDATE;
				}

				/* Detect writes whose destination address is inside the
				 * current block, using constant propagation. When these
				 * occur, we mark the blocks as not compilable. */
				if ((known & BIT(list->i.rs)) &&
				    kunseg(values[list->i.rs]) >= kunseg(block->pc) &&
				    kunseg(values[list->i.rs]) < (kunseg(block->pc) +
								  block->nb_ops * 4)) {
					pr_debug("Self-modifying block detected\n");
					block->flags |= BLOCK_NEVER_COMPILE;
					list->flags |= LIGHTREC_SMC;
				}
			}
		case OP_SWL: /* fall-through */
		case OP_SWR:
		case OP_SWC2:
		case OP_LB:
		case OP_LBU:
		case OP_LH:
		case OP_LHU:
		case OP_LW:
		case OP_LWL:
		case OP_LWR:
		case OP_LWC2:
			if (OPT_FLAG_IO && (known & BIT(list->i.rs))) {
				val = kunseg(values[list->i.rs] + (s16) list->i.imm);
				map = lightrec_get_map(state, NULL, val);

				if (!map || map->ops ||
				    map == &state->maps[PSX_MAP_PARALLEL_PORT]) {
					pr_debug("Flagging opcode %u as accessing I/O registers\n",
						 i);
					list->flags |= LIGHTREC_HW_IO;
				} else {
					pr_debug("Flaging opcode %u as direct memory access\n", i);
					list->flags |= LIGHTREC_DIRECT_IO;
				}
			}
		default: /* fall-through */
			break;
		}

		known = lightrec_propagate_consts(list, known, values);
	}

	return 0;
}

static u8 get_mfhi_mflo_reg(const struct block *block, u16 offset,
			    const struct opcode *last,
			    u32 mask, bool sync, bool mflo, bool another)
{
	const struct opcode *op, *next = &block->opcode_list[offset];
	u32 old_mask;
	u8 reg2, reg = mflo ? REG_LO : REG_HI;
	u16 branch_offset;
	unsigned int i;

	for (i = offset; i < block->nb_ops; i++) {
		op = next;
		next = &block->opcode_list[i + 1];
		old_mask = mask;

		/* If any other opcode writes or reads to the register
		 * we'd use, then we cannot use it anymore. */
		mask |= opcode_read_mask(op->c);
		mask |= opcode_write_mask(op->c);

		if (op->flags & LIGHTREC_SYNC)
			sync = true;

		switch (op->i.op) {
		case OP_BEQ:
		case OP_BNE:
		case OP_BLEZ:
		case OP_BGTZ:
		case OP_REGIMM:
			/* TODO: handle backwards branches too */
			if (!last &&
			    (op->flags & LIGHTREC_LOCAL_BRANCH) &&
			    (s16)op->c.i.imm >= 0) {
				branch_offset = i + 1 + (s16)op->c.i.imm
					- !!(OPT_SWITCH_DELAY_SLOTS && (op->flags & LIGHTREC_NO_DS));

				reg = get_mfhi_mflo_reg(block, branch_offset, NULL,
							mask, sync, mflo, false);
				reg2 = get_mfhi_mflo_reg(block, offset + 1, next,
							 mask, sync, mflo, false);
				if (reg > 0 && reg == reg2)
					return reg;
				if (!reg && !reg2)
					return 0;
			}

			return mflo ? REG_LO : REG_HI;
		case OP_SPECIAL:
			switch (op->r.op) {
			case OP_SPECIAL_MULT:
			case OP_SPECIAL_MULTU:
			case OP_SPECIAL_DIV:
			case OP_SPECIAL_DIVU:
				return 0;
			case OP_SPECIAL_MTHI:
				if (!mflo)
					return 0;
				continue;
			case OP_SPECIAL_MTLO:
				if (mflo)
					return 0;
				continue;
			case OP_SPECIAL_JR:
				if (op->r.rs != 31)
					return reg;

				if (!sync &&
				    !(op->flags & LIGHTREC_NO_DS) &&
				    (next->i.op == OP_SPECIAL) &&
				    ((!mflo && next->r.op == OP_SPECIAL_MFHI) ||
				    (mflo && next->r.op == OP_SPECIAL_MFLO)))
					return next->r.rd;

				return 0;
			case OP_SPECIAL_JALR:
				return reg;
			case OP_SPECIAL_MFHI:
				if (!mflo) {
					if (another)
						return op->r.rd;
					/* Must use REG_HI if there is another MFHI target*/
					reg2 = get_mfhi_mflo_reg(block, i + 1, next,
							 0, sync, mflo, true);
					if (reg2 > 0 && reg2 != REG_HI)
						return REG_HI;

					if (!sync && !(old_mask & BIT(op->r.rd)))
						return op->r.rd;
					else
						return REG_HI;
				}
				continue;
			case OP_SPECIAL_MFLO:
				if (mflo) {
					if (another)
						return op->r.rd;
					/* Must use REG_LO if there is another MFLO target*/
					reg2 = get_mfhi_mflo_reg(block, i + 1, next,
							 0, sync, mflo, true);
					if (reg2 > 0 && reg2 != REG_LO)
						return REG_LO;

					if (!sync && !(old_mask & BIT(op->r.rd)))
						return op->r.rd;
					else
						return REG_LO;
				}
				continue;
			default:
				break;
			}

			/* fall-through */
		default:
			continue;
		}
	}

	return reg;
}

static void lightrec_replace_lo_hi(struct block *block, u16 offset,
				   u16 last, bool lo)
{
	unsigned int i;
	u32 branch_offset;

	/* This function will remove the following MFLO/MFHI. It must be called
	 * only if get_mfhi_mflo_reg() returned a non-zero value. */

	for (i = offset; i < last; i++) {
		struct opcode *op = &block->opcode_list[i];

		switch (op->i.op) {
		case OP_BEQ:
		case OP_BNE:
		case OP_BLEZ:
		case OP_BGTZ:
		case OP_REGIMM:
			/* TODO: handle backwards branches too */
			if ((op->flags & LIGHTREC_LOCAL_BRANCH) &&
			    (s16)op->c.i.imm >= 0) {
				branch_offset = i + 1 + (s16)op->c.i.imm
					- !!(OPT_SWITCH_DELAY_SLOTS && (op->flags & LIGHTREC_NO_DS));

				lightrec_replace_lo_hi(block, branch_offset, last, lo);
				lightrec_replace_lo_hi(block, i + 1, branch_offset, lo);
			}
			break;

		case OP_SPECIAL:
			if (lo && op->r.op == OP_SPECIAL_MFLO) {
				pr_debug("Removing MFLO opcode at offset 0x%x\n",
					 i << 2);
				op->opcode = 0;
				return;
			} else if (!lo && op->r.op == OP_SPECIAL_MFHI) {
				pr_debug("Removing MFHI opcode at offset 0x%x\n",
					 i << 2);
				op->opcode = 0;
				return;
			}

			/* fall-through */
		default:
			break;
		}
	}
}

static int lightrec_flag_mults_divs(struct lightrec_state *state, struct block *block)
{
	struct opcode *list;
	u8 reg_hi, reg_lo;
	unsigned int i;

	for (i = 0; i < block->nb_ops - 1; i++) {
		list = &block->opcode_list[i];

		if (list->i.op != OP_SPECIAL)
			continue;

		switch (list->r.op) {
		case OP_SPECIAL_MULT:
		case OP_SPECIAL_MULTU:
		case OP_SPECIAL_DIV:
		case OP_SPECIAL_DIVU:
			break;
		default:
			continue;
		}

		/* Don't support opcodes in delay slots */
		if ((i && has_delay_slot(block->opcode_list[i - 1].c)) ||
		    (list->flags & LIGHTREC_NO_DS))
			continue;

		reg_lo = get_mfhi_mflo_reg(block, i + 1, NULL, 0, false, true, false);
		if (reg_lo == 0) {
			pr_debug("Mark MULT(U)/DIV(U) opcode at offset 0x%x as"
				 " not writing LO\n", i << 2);
			list->flags |= LIGHTREC_NO_LO;
		}

		reg_hi = get_mfhi_mflo_reg(block, i + 1, NULL, 0, false, false, false);
		if (reg_hi == 0) {
			pr_debug("Mark MULT(U)/DIV(U) opcode at offset 0x%x as"
				 " not writing HI\n", i << 2);
			list->flags |= LIGHTREC_NO_HI;
		}

		if (!reg_lo && !reg_hi) {
			pr_debug("Both LO/HI unused in this block, they will "
				 "probably be used in parent block - removing "
				 "flags.\n");
			list->flags &= ~(LIGHTREC_NO_LO | LIGHTREC_NO_HI);
		}

		if (reg_lo > 0 && reg_lo != REG_LO) {
			pr_debug("Found register %s to hold LO (rs = %u, rt = %u)\n",
				 lightrec_reg_name(reg_lo), list->r.rs, list->r.rt);

			lightrec_replace_lo_hi(block, i + 1, block->nb_ops, true);
			list->r.rd = reg_lo;
		} else {
			list->r.rd = 0;
		}

		if (reg_hi > 0 && reg_hi != REG_HI) {
			pr_debug("Found register %s to hold HI (rs = %u, rt = %u)\n",
				 lightrec_reg_name(reg_hi), list->r.rs, list->r.rt);

			lightrec_replace_lo_hi(block, i + 1, block->nb_ops, false);
			list->r.imm = reg_hi;
		} else {
			list->r.imm = 0;
		}
	}

	return 0;
}

static bool remove_div_sequence(struct block *block, unsigned int offset)
{
	struct opcode *op;
	unsigned int i, found = 0;

	/*
	 * Scan for the zero-checking sequence that GCC automatically introduced
	 * after most DIV/DIVU opcodes. This sequence checks the value of the
	 * divisor, and if zero, executes a BREAK opcode, causing the BIOS
	 * handler to crash the PS1.
	 *
	 * For DIV opcodes, this sequence additionally checks that the signed
	 * operation does not overflow.
	 *
	 * With the assumption that the games never crashed the PS1, we can
	 * therefore assume that the games never divided by zero or overflowed,
	 * and these sequences can be removed.
	 */

	for (i = offset; i < block->nb_ops; i++) {
		op = &block->opcode_list[i];

		if (!found) {
			if (op->i.op == OP_SPECIAL &&
			    (op->r.op == OP_SPECIAL_DIV || op->r.op == OP_SPECIAL_DIVU))
				break;

			if ((op->opcode & 0xfc1fffff) == 0x14000002) {
				/* BNE ???, zero, +8 */
				found++;
			} else {
				offset++;
			}
		} else if (found == 1 && !op->opcode) {
			/* NOP */
			found++;
		} else if (found == 2 && op->opcode == 0x0007000d) {
			/* BREAK 0x1c00 */
			found++;
		} else if (found == 3 && op->opcode == 0x2401ffff) {
			/* LI at, -1 */
			found++;
		} else if (found == 4 && (op->opcode & 0xfc1fffff) == 0x14010004) {
			/* BNE ???, at, +16 */
			found++;
		} else if (found == 5 && op->opcode == 0x3c018000) {
			/* LUI at, 0x8000 */
			found++;
		} else if (found == 6 && (op->opcode & 0x141fffff) == 0x14010002) {
			/* BNE ???, at, +16 */
			found++;
		} else if (found == 7 && !op->opcode) {
			/* NOP */
			found++;
		} else if (found == 8 && op->opcode == 0x0006000d) {
			/* BREAK 0x1800 */
			found++;
			break;
		} else {
			break;
		}
	}

	if (found >= 3) {
		if (found != 9)
			found = 3;

		pr_debug("Removing DIV%s sequence at offset 0x%x\n",
			 found == 9 ? "" : "U", offset << 2);

		for (i = 0; i < found; i++)
			block->opcode_list[offset + i].opcode = 0;

		return true;
	}

	return false;
}

static int lightrec_remove_div_by_zero_check_sequence(struct lightrec_state *state,
						      struct block *block)
{
	struct opcode *op;
	unsigned int i;

	for (i = 0; i < block->nb_ops; i++) {
		op = &block->opcode_list[i];

		if (op->i.op == OP_SPECIAL &&
		    (op->r.op == OP_SPECIAL_DIVU || op->r.op == OP_SPECIAL_DIV) &&
		    remove_div_sequence(block, i + 1))
			op->flags |= LIGHTREC_NO_DIV_CHECK;
	}

	return 0;
}

static const u32 memset_code[] = {
	0x10a00006,	// beqz		a1, 2f
	0x24a2ffff,	// addiu	v0,a1,-1
	0x2403ffff,	// li		v1,-1
	0xac800000,	// 1: sw	zero,0(a0)
	0x2442ffff,	// addiu	v0,v0,-1
	0x1443fffd,	// bne		v0,v1, 1b
	0x24840004,	// addiu	a0,a0,4
	0x03e00008,	// 2: jr	ra
	0x00000000,	// nop
};

static int lightrec_replace_memset(struct lightrec_state *state, struct block *block)
{
	unsigned int i;
	union code c;

	for (i = 0; i < block->nb_ops; i++) {
		c = block->opcode_list[i].c;

		if (c.opcode != memset_code[i])
			return 0;

		if (i == ARRAY_SIZE(memset_code) - 1) {
			/* success! */
			pr_debug("Block at PC 0x%x is a memset\n", block->pc);
			block->flags |= BLOCK_IS_MEMSET | BLOCK_NEVER_COMPILE;

			/* Return non-zero to skip other optimizers. */
			return 1;
		}
	}

	return 0;
}

static int (*lightrec_optimizers[])(struct lightrec_state *state, struct block *) = {
	IF_OPT(OPT_REMOVE_DIV_BY_ZERO_SEQ, &lightrec_remove_div_by_zero_check_sequence),
	IF_OPT(OPT_REPLACE_MEMSET, &lightrec_replace_memset),
	IF_OPT(OPT_DETECT_IMPOSSIBLE_BRANCHES, &lightrec_detect_impossible_branches),
	IF_OPT(OPT_LOCAL_BRANCHES, &lightrec_local_branches),
	IF_OPT(OPT_CONDITIONAL_MOVES, &lightrec_conditional_moves),
	IF_OPT(OPT_TRANSFORM_OPS, &lightrec_transform_ops),
	IF_OPT(OPT_SWITCH_DELAY_SLOTS, &lightrec_switch_delay_slots),
	IF_OPT(OPT_FLAG_IO || OPT_FLAG_STORES, &lightrec_flag_io),
	IF_OPT(OPT_FLAG_MULT_DIV, &lightrec_flag_mults_divs),
	IF_OPT(OPT_EARLY_UNLOAD, &lightrec_early_unload),
};

int lightrec_optimize(struct lightrec_state *state, struct block *block)
{
	unsigned int i;
	int ret;

	for (i = 0; i < ARRAY_SIZE(lightrec_optimizers); i++) {
		if (lightrec_optimizers[i]) {
			ret = (*lightrec_optimizers[i])(state, block);
			if (ret)
				return ret;
		}
	}

	return 0;
}
