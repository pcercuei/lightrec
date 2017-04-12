/*
 * Copyright (C) 2017 Paul Cercueil <paul@crapouillou.net>
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
#include "lightrec-private.h"

void lightrec_lw(struct lightrec_state *state, const struct opcode *op)
{
	const struct lightrec_mem_map *map;
	u32 val, kaddr, addr, offset;

	addr = state->native_reg_cache[op->i.rs] + (s16) op->i.imm;
	kaddr = kunseg(addr);
	map = lightrec_find_map(state, kaddr);

	if (unlikely(!map))
		return;

	offset = kaddr - map->pc;

	while (map->mirror_of)
		map = map->mirror_of;

	if (unlikely(map->ops))
		val = map->ops->lw(state, op, addr);
	else
		val = *(u32 *)((uintptr_t) map->address + offset);

	if (likely(op->i.rt > 0))
		state->native_reg_cache[op->i.rt] = val;
}
