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

#define ARRAY_SIZE(x) (sizeof(x) ? sizeof(x) / sizeof((x)[0]) : 0)
#define BIT(x) (1 << (x))

#ifdef __GNUC__
#	define likely(x)       __builtin_expect(!!(x),1)
#	define unlikely(x)     __builtin_expect(!!(x),0)
#else
#	define likely(x)       (x)
#	define unlikely(x)     (x)
#endif

#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
#	define LE32TOH(x)	__builtin_bswap32(x)
#	define HTOLE32(x)	__builtin_bswap32(x)
#	define LE16TOH(x)	__builtin_bswap16(x)
#	define HTOLE16(x)	__builtin_bswap16(x)
#else
#	define LE32TOH(x)	(x)
#	define HTOLE32(x)	(x)
#	define LE16TOH(x)	(x)
#	define HTOLE16(x)	(x)
#endif

/* Flags for (struct block *)->flags */
#define BLOCK_NEVER_COMPILE	BIT(0)

/* Definition of jit_state_t (avoids inclusion of <lightning.h>) */
struct jit_state;
typedef struct jit_state jit_state_t;

struct blockcache;
struct recompiler;
struct regcache;
struct opcode;

struct block {
	jit_state_t *_jit;
	struct lightrec_state *state;
	struct opcode *opcode_list;
	void (*function)(void);
	const u32 *code;
	u32 pc, kunseg_pc;
	unsigned int cycles;
	unsigned int length;
	const struct lightrec_mem_map *map;
	struct {
		struct block *sle_next;
	} next;
	uintptr_t flags;
};

struct lightrec_op_data {
	struct opcode *op;
	u32 addr;
	u32 data;
};

struct lightrec_state {
	u32 native_reg_cache[34];
	u32 next_pc;
	u32 current_cycle;
	u32 target_cycle;
	u32 exit_flags;
	struct lightrec_op_data op_data;
	struct block *wrapper, *rw_wrapper, *mfc_wrapper, *mtc_wrapper,
		     *rfe_wrapper, *cp_wrapper;
	struct blockcache *block_cache;
	struct regcache *reg_cache;
	struct recompiler *rec;
	void (*eob_wrapper_func)(void);
	void (*get_next_block)(void);
	struct lightrec_ops ops;
	unsigned int nb_maps;
	const struct lightrec_mem_map *maps;
	uintptr_t offset_ram, offset_bios, offset_scratch;
	_Bool mirrors_mapped;
	void *code_lut[];
};

u32 lightrec_rw(struct lightrec_state *state,
		struct opcode *op, u32 addr, u32 data);

void lightrec_free_block(struct block *block);

static inline u32 kunseg(u32 addr)
{
	if (unlikely(addr >= 0xa0000000))
		return addr - 0xa0000000;
	else if (addr >= 0x80000000)
		return addr - 0x80000000;
	else
		return addr;
}

void lightrec_mtc(struct lightrec_state *state,
		  const struct opcode *op, u32 data);
u32 lightrec_mfc(struct lightrec_state *state, const struct opcode *op);

struct block * lightrec_get_block(struct lightrec_state *state, u32 pc);
int lightrec_compile_block(struct block *block);

#endif /* __LIGHTREC_PRIVATE_H__ */
