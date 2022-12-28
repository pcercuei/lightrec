// SPDX-License-Identifier: LGPL-2.1-or-later
/*
 * Copyright (C) 2022 Paul Cercueil <paul@crapouillou.net>
 */

#include "constprop.h"
#include "disassembler.h"
#include "lightrec-private.h"

#include <string.h>

void lightrec_consts_propagate(const struct opcode *op,
			       const struct opcode *prev,
			       struct constprop_data *v)
{
	union code c = prev->c;

	/* Register $zero is always, well, zero */
	v[0].value = 0;
	v[0].sign = 0;
	v[0].known = 0xffffffff;

	if (op_flag_sync(op->flags)) {
		memset(&v[1], 0, sizeof(*v) * 31);
		return;
	}

	switch (c.i.op) {
	case OP_SPECIAL:
		switch (c.r.op) {
		case OP_SPECIAL_SLL:
			if (is_known(v, c.r.rt)) {
				v[c.r.rd].known = 0xffffffff;
				v[c.r.rd].value = v[c.r.rt].value << c.r.imm;
			} else {
				v[c.r.rd].known = 0;
			}
			break;

		case OP_SPECIAL_SRL:
			if (is_known(v, c.r.rt)) {
				v[c.r.rd].known = 0xffffffff;
				v[c.r.rd].value = v[c.r.rt].value >> c.r.imm;
			} else {
				v[c.r.rd].known = 0;
			}
			break;

		case OP_SPECIAL_SRA:
			if (is_known(v, c.r.rt)) {
				v[c.r.rd].known = 0xffffffff;
				v[c.r.rd].value = (s32)v[c.r.rt].value >> c.r.imm;
			} else {
				v[c.r.rd].known = 0;
			}
			break;

		case OP_SPECIAL_SLLV:
			if (is_known(v, c.r.rs) && is_known(v, c.r.rt)) {
				v[c.r.rd].known = 0xffffffff;
				v[c.r.rd].value = v[c.r.rt].value >> (v[c.r.rs].value & 0x1f);
			} else {
				v[c.r.rd].known = 0;
			}
			break;

		case OP_SPECIAL_SRLV:
			if (is_known(v, c.r.rs) && is_known(v, c.r.rt)) {
				v[c.r.rd].known = 0xffffffff;
				v[c.r.rd].value = v[c.r.rt].value >> (v[c.r.rs].value & 0x1f);
			} else {
				v[c.r.rd].known = 0;
			}
			break;

		case OP_SPECIAL_SRAV:
			if (is_known(v, c.r.rs) && is_known(v, c.r.rt)) {
				v[c.r.rd].known = 0xffffffff;
				v[c.r.rd].value = (s32)v[c.r.rt].value >> (v[c.r.rs].value & 0x1f);
			} else {
				v[c.r.rd].known = 0;
			}
			break;

		case OP_SPECIAL_ADD:
		case OP_SPECIAL_ADDU:
			if (is_known(v, c.r.rt) && is_known(v, c.r.rs)) {
				v[c.r.rd].value = v[c.r.rt].value + v[c.r.rs].value;
				v[c.r.rd].known = 0xffffffff;
			} else {
				v[c.r.rd].known = 0;
			}
			v[c.r.rd].sign = 0;
			break;

		case OP_SPECIAL_SUB:
		case OP_SPECIAL_SUBU:
			if (is_known(v, c.r.rt) && is_known(v, c.r.rs)) {
				v[c.r.rd].value = v[c.r.rt].value - v[c.r.rs].value;
				v[c.r.rd].known = 0xffffffff;
			} else {
				v[c.r.rd].known = 0;
			}
			v[c.r.rd].sign = 0;
			break;

		case OP_SPECIAL_AND:
			if (is_known(v, c.r.rt) && is_known(v, c.r.rs)) {
				v[c.r.rd].value = v[c.r.rt].value & v[c.r.rs].value;
				v[c.r.rd].known = 0xffffffff;
			} else {
				v[c.r.rd].known = 0;
			}
			break;

		case OP_SPECIAL_OR:
			if (is_known(v, c.r.rt) && is_known(v, c.r.rs)) {
				v[c.r.rd].value = v[c.r.rt].value | v[c.r.rs].value;
				v[c.r.rd].known = 0xffffffff;
			} else {
				v[c.r.rd].known = 0;
			}
			break;

		case OP_SPECIAL_XOR:
			if (is_known(v, c.r.rt) && is_known(v, c.r.rs)) {
				v[c.r.rd].value = v[c.r.rt].value ^ v[c.r.rs].value;
				v[c.r.rd].known = 0xffffffff;
			} else {
				v[c.r.rd].known = 0;
			}
			break;

		case OP_SPECIAL_NOR:
			if (is_known(v, c.r.rt) && is_known(v, c.r.rs)) {
				v[c.r.rd].value = ~(v[c.r.rt].value | v[c.r.rs].value);
				v[c.r.rd].known = 0xffffffff;
			} else {
				v[c.r.rd].known = 0;
			}
			break;

		case OP_SPECIAL_SLT:
			if (is_known(v, c.r.rt) && is_known(v, c.r.rs)) {
				v[c.r.rd].value = (s32)v[c.r.rs].value < (s32)v[c.r.rt].value;
				v[c.r.rd].known = 0xffffffff;
			} else {
				v[c.r.rd].known = 0;
			}
			break;
		case OP_SPECIAL_SLTU:
			if (is_known(v, c.r.rt) && is_known(v, c.r.rs)) {
				v[c.r.rd].value = v[c.r.rs].value < v[c.r.rt].value;
				v[c.r.rd].known = 0xffffffff;
			} else {
				v[c.r.rd].known = 0;
			}
			break;
		case OP_SPECIAL_MULT:
		case OP_SPECIAL_MULTU:
		case OP_SPECIAL_DIV:
		case OP_SPECIAL_DIVU:
			if (OPT_FLAG_MULT_DIV && c.r.rd) {
				v[c.r.rd].known = 0;
				v[c.r.rd].sign = 0;
			}
			if (OPT_FLAG_MULT_DIV && c.r.imm) {
				v[c.r.imm].known = 0;
				v[c.r.imm].sign = 0;
			}
			break;

		case OP_SPECIAL_MFLO:
		case OP_SPECIAL_MFHI:
			v[c.r.rd].known = 0;
			v[c.r.rd].sign = 0;
			break;
		default:
			break;
		}
		break;

	case OP_META_MULT2:
	case OP_META_MULTU2:
		if (OPT_FLAG_MULT_DIV && c.r.rd) {
			if (!is_known(v, c.r.rs)) {
				v[c.r.rd].known = 0;
			} else if (c.r.op < 32) {
				v[c.r.rd].value = v[c.r.rs].value << c.r.op;
				v[c.r.rd].known = 0xffffffff;
			} else {
				v[c.r.rd].value = 0;
				v[c.r.rd].known = 0xffffffff;
			}
		}

		if (OPT_FLAG_MULT_DIV && c.r.imm) {
			if (!is_known(v, c.r.rs)) {
				v[c.r.rd].known = 0;
			} else if (c.r.op >= 32) {
				v[c.r.rd].value = v[c.r.rs].value << c.r.op - 32;
				v[c.r.rd].known = 0xffffffff;
			} else if (c.i.op == OP_META_MULT2) {
				v[c.r.rd].value = (s32)v[c.r.rs].value >> 32 - c.r.op;
				v[c.r.rd].known = 0xffffffff;
			} else {
				v[c.r.rd].value = v[c.r.rs].value >> 32 - c.r.op;
				v[c.r.rd].known = 0xffffffff;
			}
		}
		break;

	case OP_REGIMM:
		break;

	case OP_ADDI:
	case OP_ADDIU:
		if (is_known(v, c.i.rs)) {
			v[c.i.rt].value = (u32)((s32)v[c.i.rs].value + (s32)(s16)c.i.imm);
			v[c.i.rt].known = 0xffffffff;
		} else {
			v[c.i.rt].known = 0;
		}
		break;

	case OP_SLTI:
		if (is_known(v, c.i.rs)) {
			v[c.i.rt].value = (s32)v[c.i.rs].value < (s32)(s16)c.i.imm;
			v[c.i.rt].known = 0xffffffff;
		} else {
			v[c.i.rt].known = 0;
		}
		break;
	case OP_SLTIU:
		if (is_known(v, c.i.rs)) {
			v[c.i.rt].value = v[c.i.rs].value < (u32)(s32)(s16)c.i.imm;
			v[c.i.rt].known = 0xffffffff;
		} else {
			v[c.i.rt].known = 0;
		}
		break;

	case OP_ANDI:
		if (is_known(v, c.i.rs)) {
			v[c.i.rt].value = v[c.i.rs].value & c.i.imm;
			v[c.i.rt].known = 0xffffffff;
		} else {
			v[c.i.rt].known = 0;
		}
		break;

	case OP_ORI:
		if (is_known(v, c.i.rs)) {
			v[c.i.rt].value = v[c.i.rs].value | c.i.imm;
			v[c.i.rt].known = 0xffffffff;
		} else {
			v[c.i.rt].known = 0;
		}
		break;

	case OP_XORI:
		if (is_known(v, c.i.rs)) {
			v[c.i.rt].value = v[c.i.rs].value ^ c.i.imm;
			v[c.i.rt].known = 0xffffffff;
		} else {
			v[c.i.rt].known = 0;
		}
		break;

	case OP_LUI:
		v[c.i.rt].value = c.i.imm << 16;
		v[c.i.rt].known = 0xffffffff;
		v[c.i.rt].sign = 0;
		break;

	case OP_CP0:
		switch (c.r.rs) {
		case OP_CP0_MFC0:
		case OP_CP0_CFC0:
			v[c.r.rt].known = 0;
			v[c.r.rt].sign = 0;
			break;
		default:
			break;
		}
		break;

	case OP_CP2:
		if (c.r.op == OP_CP2_BASIC) {
			switch (c.r.rs) {
			case OP_CP2_BASIC_MFC2:
			case OP_CP2_BASIC_CFC2:
				/* TODO: Some registers are 16-bit */
				v[c.r.rt].known = 0;
				v[c.r.rt].sign = 0;
				break;
			}
		}
		break;
	case OP_LB:
		v[c.i.rt].known = 0;
		v[c.i.rt].sign = 0xffffff80;
		break;
	case OP_LH:
		v[c.i.rt].known = 0;
		v[c.i.rt].sign = 0xffff8000;
		break;
	case OP_LBU:
		v[c.i.rt].value = 0;
		v[c.i.rt].known = 0xffffff00;
		v[c.i.rt].sign = 0;
		break;
	case OP_LHU:
		v[c.i.rt].value = 0;
		v[c.i.rt].known = 0xffff0000;
		v[c.i.rt].sign = 0;
		break;
	case OP_LWL:
	case OP_LWR:
		/* TODO: LWL/LWR don't write the full register if the address is
		 * unaligned, so we only need to know the low 2 bits */
		fallthrough;
	case OP_LW:
		v[c.i.rt].known = 0;
		v[c.i.rt].sign = 0;
		break;
	case OP_META_MOV:
		v[c.r.rd] = v[c.r.rs];
		break;
	case OP_META_EXTC:
		if (is_known(v, c.i.rs)) {
			v[c.i.rt].value = (s32)(s8)v[c.i.rs].value;
			v[c.i.rt].known = 0xffffffff;
		} else {
			v[c.i.rt].known = 0;
		}
		break;
	case OP_META_EXTS:
		if (is_known(v, c.i.rs)) {
			v[c.i.rt].value = (s32)(s16)v[c.i.rs].value;
			v[c.i.rt].known = 0xffffffff;
		} else {
			v[c.i.rt].known = 0;
		}
		break;
	default:
		break;
	}

	/* Reset register 0 which may have been used as a target */
	v[0].value = 0;
	v[0].sign = 0;
	v[0].known = 0xffffffff;
}
