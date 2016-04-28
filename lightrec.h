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

#ifndef __LIGHTREC_H__
#define __LIGHTREC_H__

#include <stdbool.h>
#include <stdint.h>

#define __weak __attribute__((weak))
#define __packed __attribute__((packed))

#define ARRAY_SIZE(x) (sizeof(x) ? sizeof(x) / sizeof((x)[0]) : 0)

typedef uint64_t u64;
typedef uint32_t u32;
typedef uint16_t u16;
typedef uint8_t  u8;

typedef int64_t s64;
typedef int32_t s32;
typedef int16_t s16;
typedef int8_t  s8;

#ifdef __GNUC__
#	define likely(x)       __builtin_expect(!!(x),1)
#	define unlikely(x)     __builtin_expect(!!(x),0)
#else
#	define likely(x)       (x)
#	define unlikely(x)     (x)
#endif

#define MAP_IS_RWX	(1 << 0)


/* Definition of jit_state_t (avoids inclusion of <lightning.h>) */
struct jit_state;
typedef struct jit_state jit_state_t;

struct opcode;
struct lightrec_state;
struct blockcache;
struct regcache;
struct lightrec_mem_map;

enum block_exit_flags {
	LIGHTREC_EXIT_NORMAL,
	LIGHTREC_EXIT_SYSCALL,
	LIGHTREC_EXIT_BREAK,
	LIGHTREC_EXIT_SEGFAULT,
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
	const struct lightrec_mem_map *map;
};

struct lightrec_mem_map_ops {
	void (*sb)(struct lightrec_state *, const struct opcode *, u32, u8);
	void (*sh)(struct lightrec_state *, const struct opcode *, u32, u16);
	void (*sw)(struct lightrec_state *, const struct opcode *, u32, u32);
	u8 (*lb)(struct lightrec_state *, const struct opcode *, u32);
	u16 (*lh)(struct lightrec_state *, const struct opcode *, u32);
	u32 (*lw)(struct lightrec_state *, const struct opcode *, u32);
};

struct lightrec_mem_map {
	u32 pc;
	u32 length;
	u32 flags;
	void *address;
	struct lightrec_mem_map_ops *ops;
	u32 *invalidation_table;
	unsigned int page_shift;
};

struct lightrec_cop_ops {
	u32 (*mfc)(struct lightrec_state *state, int cp, u8 reg);
	u32 (*cfc)(struct lightrec_state *state, int cp, u8 reg);
	void (*mtc)(struct lightrec_state *state, int cp, u8 reg, u32 value);
	void (*ctc)(struct lightrec_state *state, int cp, u8 reg, u32 value);
	void (*op)(struct lightrec_state *state, int cp, u32 func);
};

struct lightrec_state {
	u32 native_reg_cache[34];
	u32 next_pc;
	u32 current_cycle;
	enum block_exit_flags block_exit_flags;
	uintptr_t end_of_block;
	struct block *wrapper, *addr_lookup_block, *current;
	struct blockcache *block_cache;
	struct regcache *reg_cache;
	void (*addr_lookup)(void);
	u32 (*rw_op)(struct lightrec_state *, const struct opcode *, u32, u32);
	const struct lightrec_cop_ops *cop_ops;
	bool stop;
	unsigned int nb_maps;
	struct lightrec_mem_map mem_map[];
};

struct block * lightrec_recompile_block(struct lightrec_state *state, u32 pc);
void lightrec_free_block(struct block *block);

struct lightrec_state * lightrec_init(char *argv0,
		struct lightrec_mem_map *map, unsigned int nb,
		const struct lightrec_cop_ops *cop_ops);
void lightrec_destroy(struct lightrec_state *state);

u32 lightrec_execute(struct lightrec_state *state, u32 pc);
void lightrec_invalidate(struct lightrec_state *state, u32 addr, u32 len);

#endif /* __LIGHTREC_H__ */
