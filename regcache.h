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

#ifndef __REGCACHE_H__
#define __REGCACHE_H__

#include "lightrec.h"

#include <stdbool.h>

#define NUM_REGS (JIT_V_NUM - 1)
#define NUM_TEMPS (JIT_R_NUM)
#define LIGHTREC_REG_STATE (JIT_V(JIT_V_NUM - 1))

struct register_value {
	bool known;
	u32 value;
};

u8 lightrec_alloc_reg_temp(jit_state_t *_jit);
u8 lightrec_alloc_reg_out(jit_state_t *_jit, u8 reg);
u8 lightrec_alloc_reg_in(jit_state_t *_jit, u8 reg);

void lightrec_regcache_reset(void);

void lightrec_free_reg(u8 jit_reg);
void lightrec_free_regs(void);
void lightrec_clean_reg(jit_state_t *_jit, u8 jit_reg);
void lightrec_storeback_regs(jit_state_t *_jit);
void lightrec_unlink_addresses(void);

u8 lightrec_alloc_reg_in_address(jit_state_t *_jit, u8 reg, s16 offset);

#endif /* __REGCACHE_H__ */
