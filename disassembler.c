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

#include <dis-asm.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "debug.h"
#include "disassembler.h"
#include "lightrec.h"

static bool is_unconditional_jump(const struct opcode *op)
{
	switch (op->i.op) {
	case OP_SPECIAL:
		return op->r.op == OP_SPECIAL_JR || op->r.op == OP_SPECIAL_JALR;
	case OP_J:
	case OP_JAL:
		return true;
	case OP_BEQ:
	case OP_BLEZ:
		return op->i.rs == op->i.rt;
	case OP_REGIMM:
		return (op->r.rt == OP_REGIMM_BGEZ) && op->i.rs == 0;
	default:
		return false;
	}
}

static bool is_syscall(const struct opcode *op)
{
	return op->i.op == OP_SPECIAL && (op->r.op == OP_SPECIAL_SYSCALL ||
			op->r.op == OP_SPECIAL_BREAK);
}

void lightrec_free_opcode_list(struct opcode *list)
{
	while (list) {
		struct opcode *next = SLIST_NEXT(list, next);
		free(list);
		list = next;
	}
}

struct opcode * lightrec_disassemble(const u32 *src)
{
	struct opcode_list_head head = { NULL };
	bool stop_next = false;
	struct opcode *curr, *last;

	for (last = NULL; ; last = curr) {
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
		curr->opcode = *src++;

		/* NOTE: The block disassembly ends after the opcode that
		 * follows an unconditional jump (delay slot) */
		if (stop_next || is_syscall(curr))
			break;
		else if (is_unconditional_jump(curr))
			stop_next = true;
	}

	return SLIST_FIRST(&head);
}

unsigned int lightrec_cycles_of_opcode(const struct opcode *op)
{
	/* TODO: Add a proper cycle counter */
	return 2;
}

unsigned int lightrec_cycles_of_block(const struct block *block,
		const struct opcode *op_end)
{
	unsigned int count;
	const struct opcode *curr;

	if (op_end)
		op_end = SLIST_NEXT(op_end, next);

	for (count = 0, curr = block->opcode_list; curr != op_end;
			curr = SLIST_NEXT(curr, next))
		count += lightrec_cycles_of_opcode(curr);

	return count;
}

#if (LOG_LEVEL >= DEBUG_L)
void lightrec_print_disassembly(const struct block *block)
{
	struct disassemble_info info;
	const struct opcode *curr;
	const u32 *code = block->code;

	memset(&info, 0, sizeof(info));
	init_disassemble_info(&info, stdout, (fprintf_ftype) fprintf);

	info.buffer = (bfd_byte *) code;
	info.buffer_vma = (bfd_vma) code;
	for (info.buffer_length = 0, curr = block->opcode_list; curr;
			curr = SLIST_NEXT(curr, next), info.buffer_length += 4);

	info.flavour = bfd_target_unknown_flavour;
	info.arch = bfd_arch_mips;
	info.mach = bfd_mach_mips3000;
	disassemble_init_for_target(&info);

	for (curr = block->opcode_list; curr; curr = SLIST_NEXT(curr, next)) {
		putc('\t', stdout);
		print_insn_little_mips((uintptr_t) code++, &info);
		putc('\n', stdout);
	}
}
#endif
