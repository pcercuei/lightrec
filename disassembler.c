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

#include "config.h"

#if ENABLE_DISASSEMBLER
#include <dis-asm.h>
#endif
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "debug.h"
#include "disassembler.h"
#include "lightrec-private.h"

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
	return (op->i.op == OP_SPECIAL && (op->r.op == OP_SPECIAL_SYSCALL ||
					   op->r.op == OP_SPECIAL_BREAK)) ||
		(op->i.op == OP_CP0 && (op->r.rs == OP_CP0_MTC0 ||
					op->r.rs == OP_CP0_CTC0) &&
		 (op->r.rd == 12 || op->r.rd == 13));
}

void lightrec_free_opcode_list(struct opcode *list)
{
	while (list) {
		struct opcode *next = SLIST_NEXT(list, next);
		free(list);
		list = next;
	}
}

struct opcode * lightrec_disassemble(const u32 *src, unsigned int *len)
{
	struct opcode_list_head head = { NULL };
	bool stop_next = false;
	struct opcode *curr, *last;
	unsigned int i;

	for (i = 0, last = NULL; ; i += sizeof(u32), last = curr) {
		curr = calloc(1, sizeof(*curr));
		if (!curr) {
			ERROR("Unable to allocate memory\n");
			lightrec_free_opcode_list(SLIST_FIRST(&head));
			return NULL;
		}

		if (!last) {
			SLIST_INSERT_HEAD(&head, curr, next);
		} else {
			SLIST_INSERT_AFTER(last, curr, next);
		}

		/* TODO: Take care of endianness */
		curr->opcode = LE32TOH(*src++);

		/* NOTE: The block disassembly ends after the opcode that
		 * follows an unconditional jump (delay slot) */
		if (stop_next || is_syscall(curr))
			break;
		else if (is_unconditional_jump(curr))
			stop_next = true;
	}

	if (len)
		*len = i + sizeof(u32);

	return SLIST_FIRST(&head);
}

unsigned int lightrec_cycles_of_opcode(const struct opcode *op)
{
	/* TODO: Add a proper cycle counter */
	if (likely(!(op->flags & LIGHTREC_SKIP_PC_UPDATE)))
		return 2;
	else
		return 0;
}

#if ENABLE_DISASSEMBLER
void lightrec_print_disassembly(const struct block *block)
{
	struct disassemble_info info;
	const u32 *code = block->code;
	unsigned int i;

	memset(&info, 0, sizeof(info));
	init_disassemble_info(&info, stdout, (fprintf_ftype) fprintf);

	info.buffer = (bfd_byte *) code;
	info.buffer_vma = (bfd_vma)(uintptr_t) code;
	info.buffer_length = block->length;
	info.flavour = bfd_target_unknown_flavour;
	info.arch = bfd_arch_mips;
	info.mach = bfd_mach_mips3000;
	disassemble_init_for_target(&info);

	for (i = 0; i < block->length; i += 4) {
		void print_insn_little_mips(bfd_vma, struct disassemble_info *);
		putc('\t', stdout);
		print_insn_little_mips((bfd_vma)(uintptr_t) code++, &info);
		putc('\n', stdout);
	}
}
#endif
