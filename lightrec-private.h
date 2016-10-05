/*
 * Copyright (C) 2016 Paul Cercueil <paul@crapouillou.net>
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

#ifndef __LIGHTREC_PRIVATE_H__
#define __LIGHTREC_PRIVATE_H__

#include "lightrec.h"

#define __weak __attribute__((weak))
#define __packed __attribute__((packed))

#define ARRAY_SIZE(x) (sizeof(x) ? sizeof(x) / sizeof((x)[0]) : 0)

#ifdef __GNUC__
#	define likely(x)       __builtin_expect(!!(x),1)
#	define unlikely(x)     __builtin_expect(!!(x),0)
#else
#	define likely(x)       (x)
#	define unlikely(x)     (x)
#endif

/* Definition of jit_state_t (avoids inclusion of <lightning.h>) */
struct jit_state;
typedef struct jit_state jit_state_t;

struct blockcache;
struct regcache;

struct lightrec_mem_map_priv {
	struct lightrec_mem_map map;
	u32 *invalidation_table;
	unsigned int page_shift;
};

struct block {
	jit_state_t *_jit;
	struct lightrec_state *state;
	struct opcode *opcode_list;
	void (*function)(void);
	const u32 *code;
	u32 pc, kunseg_pc;
	u32 hash;
	unsigned int cycles;
	unsigned int length;
	const struct lightrec_mem_map_priv *map;
};

struct lightrec_state {
	u32 native_reg_cache[34];
	u32 next_pc;
	u32 current_cycle;
	u32 exit_flags;
	uintptr_t end_of_block;
	struct block *wrapper, *addr_lookup_block, *current;
	struct blockcache *block_cache;
	struct regcache *reg_cache;
	void (*addr_lookup)(void);
	const struct lightrec_cop_ops *cop_ops;
	bool stop;
	unsigned int nb_maps;
	struct lightrec_mem_map_priv *mem_map;
};

u32 lightrec_rw(struct lightrec_state *state,
		const struct opcode *op, u32 addr, u32 data);

struct block * lightrec_recompile_block(struct lightrec_state *state, u32 pc);
void lightrec_free_block(struct block *block);

#endif /* __LIGHTREC_PRIVATE_H__ */
