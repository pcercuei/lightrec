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

/* Definition of jit_state_t (avoids inclusion of <lightning.h>) */
struct jit_state;
typedef struct jit_state jit_state_t;

struct opcode_list;

enum block_exit_flags {
	LIGHTREC_EXIT_NORMAL,
	LIGHTREC_EXIT_SYSCALL,
	LIGHTREC_EXIT_BREAK,
};

struct block {
	jit_state_t *_jit;
	struct opcode_list *opcode_list;
	void (*function)(void);
	u32 pc;
	unsigned int cycles;
};

struct lightrec_mem_map {
	u32 pc;
	u32 length;
	void *address;
};

struct lightrec_state {
	u32 reg_cache[34];
	u32 next_pc;
	u32 block_exit_cycles;
	enum block_exit_flags block_exit_flags;
	uintptr_t end_of_block;
	struct block *wrapper, *addr_lookup_block;
	struct blockcache *block_cache;
	void (*addr_lookup)(void);
	bool stop;
	unsigned int nb_maps;
	struct lightrec_mem_map mem_map[];
};

struct block * lightrec_recompile_block(struct lightrec_state *state, u32 pc);
void lightrec_free_block(struct block *block);

struct lightrec_state * lightrec_init(char *argv0,
		struct lightrec_mem_map *map, unsigned int nb);
void lightrec_destroy(struct lightrec_state *state);

u32 lightrec_execute(struct lightrec_state *state, u32 pc);

#endif /* __LIGHTREC_H__ */
