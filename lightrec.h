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

#ifndef __LIGHTREC_H__
#define __LIGHTREC_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef uint64_t u64;
typedef uint32_t u32;
typedef uint16_t u16;
typedef uint8_t  u8;

typedef int64_t s64;
typedef int32_t s32;
typedef int16_t s16;
typedef int8_t  s8;

struct opcode;
struct lightrec_state;
struct lightrec_mem_map;

/* Exit flags */
#define LIGHTREC_EXIT_NORMAL	(0)
#define LIGHTREC_EXIT_SYSCALL	(1 << 0)
#define LIGHTREC_EXIT_BREAK	(1 << 1)
#define LIGHTREC_EXIT_CHECK_INTERRUPT	(1 << 2)
#define LIGHTREC_EXIT_SEGFAULT	(1 << 3)

/* Flags for lightrec_mem_map */
#define MAP_IS_RWX	(1 << 0)

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
	const struct lightrec_mem_map_ops *ops;
	const struct lightrec_mem_map *mirror_of;
};

struct lightrec_cop_ops {
	u32 (*mfc)(struct lightrec_state *state, int cp, u8 reg);
	u32 (*cfc)(struct lightrec_state *state, int cp, u8 reg);
	void (*mtc)(struct lightrec_state *state, int cp, u8 reg, u32 value);
	void (*ctc)(struct lightrec_state *state, int cp, u8 reg, u32 value);
	void (*op)(struct lightrec_state *state, int cp, u32 func);
};

struct lightrec_state * lightrec_init(char *argv0,
		const struct lightrec_mem_map *map, size_t nb,
		const struct lightrec_cop_ops *cop_ops);
void lightrec_destroy(struct lightrec_state *state);

u32 lightrec_execute(struct lightrec_state *state, u32 pc, u32 target_cycle);
void lightrec_invalidate(struct lightrec_state *state, u32 addr, u32 len);

void lightrec_set_exit_flags(struct lightrec_state *state, u32 flags);
u32 lightrec_exit_flags(struct lightrec_state *state);

void lightrec_dump_registers(struct lightrec_state *state, u32 regs[34]);
void lightrec_restore_registers(struct lightrec_state *state, u32 regs[34]);

u32 lightrec_current_cycle_count(const struct lightrec_state *state,
		const struct opcode *op);
void lightrec_reset_cycle_count(struct lightrec_state *state, u32 cycles);

#ifdef __cplusplus
};
#endif

#endif /* __LIGHTREC_H__ */
