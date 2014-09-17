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

#include <stdbool.h>
#include <stdlib.h>

#include "debug.h"
#include "disassembler.h"

static bool is_unconditional_jump(union opcode op)
{
	switch (op.i.op) {
	case OP_SPECIAL:
		return op.r.op == OP_SPECIAL_JR || op.r.op == OP_SPECIAL_JALR;
	case OP_J:
	case OP_JAL:
		return true;
	case OP_BEQ:
	case OP_BLEZ:
		return op.i.rs == op.i.rt;
	case OP_REGIMM:
		return (op.r.rt == OP_REGIMM_BGEZ) && op.i.rs == 0;
	default:
		return false;
	}
}

void lightrec_free_opcode_list(struct opcode_list *list)
{
	while (list) {
		struct opcode_list *next = SLIST_NEXT(list, next);
		free(list);
		list = next;
	}
}

struct opcode_list * lightrec_disassemble(const u32 *src)
{
	struct opcode_list_head head = { NULL };
	bool stop_next = false;
	struct opcode_list *curr, *last;

	for (last = NULL; ; last = curr) {
		union opcode op;

		curr = malloc(sizeof(*curr));
		if (!curr) {
			ERROR("Unable to allocate memory\n");
			lightrec_free_opcode_list(SLIST_FIRST(&head));
			return NULL;
		}

		if (!last) {
			SLIST_NEXT(curr, next) = NULL;
			SLIST_INSERT_HEAD(&head, curr, next);
		} else {
			SLIST_INSERT_AFTER(last, curr, next);
		}

		/* TODO: Take care of endianness */
		op.opcode = *src++;
		curr->opcode.opcode = op.opcode;

		/* NOTE: The block disassembly ends after the opcode that
		 * follows an unconditional jump (delay slot) */
		if (stop_next)
			break;
		else if (is_unconditional_jump(op))
			stop_next = true;
	}

	return SLIST_FIRST(&head);
}
