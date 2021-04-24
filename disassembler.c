// SPDX-License-Identifier: LGPL-2.1-or-later
/*
 * Copyright (C) 2014-2021 Paul Cercueil <paul@crapouillou.net>
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
#include "memmanager.h"

static bool is_unconditional_jump(union code c)
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

static bool is_syscall(union code c)
{
	return (c.i.op == OP_SPECIAL && c.r.op == OP_SPECIAL_SYSCALL) ||
		(c.i.op == OP_CP0 && (c.r.rs == OP_CP0_MTC0 ||
					c.r.rs == OP_CP0_CTC0) &&
		 (c.r.rd == 12 || c.r.rd == 13));
}

void lightrec_free_opcode_list(struct block *block)
{
	lightrec_free(block->state, MEM_FOR_IR,
		      sizeof(*block->opcode_list) * block->nb_ops,
		      block->opcode_list);
}

unsigned int lightrec_get_mips_block_len(const u32 *src)
{
	unsigned int i;
	union code c;

	for (i = 1; ; i++) {
		c.opcode = LE32TOH(*src++);

		if (is_syscall(c))
			return i;

		if (is_unconditional_jump(c))
			return i + 1;
	}
}

struct opcode * lightrec_disassemble(struct lightrec_state *state,
				     const u32 *src, unsigned int *len)
{
	struct opcode *list;
	unsigned int i, length;

	length = lightrec_get_mips_block_len(src);

	list = lightrec_malloc(state, MEM_FOR_IR, sizeof(*list) * length);
	if (!list) {
		pr_err("Unable to allocate memory\n");
		return NULL;
	}

	for (i = 0; i < length; i++) {
		list[i].opcode = LE32TOH(src[i]);
		list[i].offset = i;
		list[i].flags = 0;
		list[i].next = &list[i + 1];
	}

	list[length - 1].next = NULL;
	*len = length * sizeof(u32);

	return list;
}

unsigned int lightrec_cycles_of_opcode(union code code)
{
	return 2;
}

#if ENABLE_DISASSEMBLER
void lightrec_print_disassembly(const struct block *block,
				const u32 *code, unsigned int length)
{
	struct disassemble_info info;
	unsigned int i;

	memset(&info, 0, sizeof(info));
	init_disassemble_info(&info, stdout, (fprintf_ftype) fprintf);

	info.buffer = (bfd_byte *) code;
	info.buffer_vma = (bfd_vma)(uintptr_t) code;
	info.buffer_length = length;
	info.flavour = bfd_target_unknown_flavour;
	info.arch = bfd_arch_mips;
	info.mach = bfd_mach_mips3000;
	disassemble_init_for_target(&info);

	for (i = 0; i < length; i += 4) {
		void print_insn_little_mips(bfd_vma, struct disassemble_info *);
		putc('\t', stdout);
		print_insn_little_mips((bfd_vma)(uintptr_t) code++, &info);
		putc('\n', stdout);
	}
}
#endif
