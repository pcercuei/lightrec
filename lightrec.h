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

#include <stdint.h>

#define __weak __attribute__((weak))
#define __packed __attribute__((packed))

#define ARRAY_SIZE(x) (sizeof(x) ? sizeof(x) / sizeof((x)[0]) : 0)

/* Definition of jit_state_t (avoids inclusion of <lightning.h>) */
struct jit_state;
typedef struct jit_state jit_state_t;

struct opcode_list;

struct block {
	jit_state_t *_jit;
	struct opcode_list *opcode_list;
	void (*function)(void);
};

struct lightrec_state {
	uint32_t reg_cache[32];
	uintptr_t end_of_block;
	struct block *current;
};

extern struct lightrec_state lightrec_state;

typedef uint64_t u64;
typedef uint32_t u32;
typedef uint16_t u16;
typedef uint8_t  u8;

typedef int64_t s64;
typedef int32_t s32;
typedef int16_t s16;
typedef int8_t  s8;

struct block * lightrec_recompile_block(const u32 *code);
void lightrec_free_block(struct block *block);

void lightrec_init(char *argv0);
void lightrec_destroy(void);

void lightrec_execute(struct block *block);

#endif /* __LIGHTREC_H__ */
