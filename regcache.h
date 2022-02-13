/* SPDX-License-Identifier: LGPL-2.1-or-later */
/*
 * Copyright (C) 2014-2021 Paul Cercueil <paul@crapouillou.net>
 */

#ifndef __REGCACHE_H__
#define __REGCACHE_H__

#include "lightrec-private.h"

#define NUM_REGS (JIT_V_NUM - 2)
#define NUM_TEMPS (JIT_R_NUM)
#define LIGHTREC_REG_STATE (JIT_V(JIT_V_NUM - 1))
#define LIGHTREC_REG_CYCLE (JIT_V(JIT_V_NUM - 2))

/* Flags for lightrec_alloc_reg_in / lightrec_alloc_reg_out. */
#define REG_EXT		BIT(0) /* register is sign-extended */
#define REG_ZEXT	BIT(1) /* register is zero-extended */
#define REG_SHADOW	BIT(2) /* request a shadow register */
#define REG_NO_SHADOW	BIT(3) /* request an original register */

struct register_value {
	_Bool known;
	u32 value;
};

struct native_register;
struct regcache;

u8 lightrec_alloc_reg(struct regcache *cache, jit_state_t *_jit, u8 jit_reg);
u8 lightrec_alloc_reg_temp(struct regcache *cache, jit_state_t *_jit);
u8 lightrec_alloc_reg_out(struct regcache *cache, jit_state_t *_jit,
			  u8 reg, u8 flags);
u8 lightrec_alloc_reg_in(struct regcache *cache, jit_state_t *_jit,
			 u8 reg, u8 flags);

u8 lightrec_request_reg_in(struct regcache *cache, jit_state_t *_jit,
			   u8 reg, u8 jit_reg);

u8 lightrec_get_reg_in_flags(struct regcache *cache, u8 jit_reg);
void lightrec_set_reg_out_flags(struct regcache *cache, u8 jit_reg, u8 flags);

void lightrec_regcache_reset(struct regcache *cache);

void lightrec_lock_reg(struct regcache *cache, jit_state_t *_jit, u8 jit_reg);
void lightrec_free_reg(struct regcache *cache, u8 jit_reg);
void lightrec_free_regs(struct regcache *cache);
void lightrec_clean_reg(struct regcache *cache, jit_state_t *_jit, u8 jit_reg);
void lightrec_clean_regs(struct regcache *cache, jit_state_t *_jit);
void lightrec_unload_reg(struct regcache *cache, jit_state_t *_jit, u8 jit_reg);
void lightrec_storeback_regs(struct regcache *cache, jit_state_t *_jit);

void lightrec_clean_reg_if_loaded(struct regcache *cache, jit_state_t *_jit,
				  u8 reg, _Bool unload);

u8 lightrec_alloc_reg_in_address(struct regcache *cache,
		jit_state_t *_jit, u8 reg, s16 offset);

struct native_register * lightrec_regcache_enter_branch(struct regcache *cache);
void lightrec_regcache_leave_branch(struct regcache *cache,
			struct native_register *regs);

struct regcache * lightrec_regcache_init(struct lightrec_state *state);
void lightrec_free_regcache(struct regcache *cache);

const char * lightrec_reg_name(u8 reg);

void lightrec_regcache_mark_live(struct regcache *cache, jit_state_t *_jit);

u32 lightrec_get_shadow_mask(const struct regcache *cache);

#endif /* __REGCACHE_H__ */
