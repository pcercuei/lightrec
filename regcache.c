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

#include "debug.h"
#include "regcache.h"

#include <lightning.h>
#include <stddef.h>

struct native_register {
	bool used, loaded, dirty, output;
	s8 emulated_register;
};

struct regcache {
	struct native_register lightrec_regs[NUM_REGS + NUM_TEMPS];
	struct native_register lightrec_regs_backup[NUM_REGS + NUM_TEMPS];
};

static inline u8 lightrec_reg_number(const struct regcache *cache,
		const struct native_register *nreg)
{
	return (u8) (((uintptr_t) nreg - (uintptr_t) cache->lightrec_regs)
			/ sizeof(*nreg));
}

static inline u8 lightrec_reg_to_lightning(const struct regcache *cache,
		const struct native_register *nreg)
{
	u8 offset = lightrec_reg_number(cache, nreg);
	return offset < NUM_REGS ? JIT_V(offset) : JIT_R(offset - NUM_REGS);
}

static inline struct native_register * lightning_reg_to_lightrec(
		struct regcache *cache, u8 reg)
{
	if ((JIT_V0 > JIT_R0 && reg >= JIT_V0) ||
			(JIT_V0 < JIT_R0 && reg < JIT_R0))
		return &cache->lightrec_regs[reg - JIT_V0];
	else
		return &cache->lightrec_regs[NUM_REGS + reg - JIT_R0];
}

static struct native_register * alloc_temp(struct regcache *cache)
{
	unsigned int i;

	/* We search the register list in reverse order. As temporaries are
	 * meant to be used only in the emitter functions, they can be mapped to
	 * caller-saved registers, as they won't have to be saved back to
	 * memory. */
	for (i = ARRAY_SIZE(cache->lightrec_regs); i; i--) {
		struct native_register *nreg = &cache->lightrec_regs[i - 1];
		if (!nreg->used && !nreg->loaded && !nreg->dirty)
			return nreg;
	}

	for (i = ARRAY_SIZE(cache->lightrec_regs); i; i--) {
		struct native_register *nreg = &cache->lightrec_regs[i - 1];
		if (!nreg->used)
			return nreg;
	}

	return NULL;
}

static struct native_register * alloc_in_out(struct regcache *cache, u8 reg)
{
	unsigned int i;

	/* Try to find if the register is already mapped somewhere */
	for (i = 0; i < ARRAY_SIZE(cache->lightrec_regs); i++) {
		struct native_register *nreg = &cache->lightrec_regs[i];
		if ((!reg || nreg->loaded || nreg->dirty) &&
				nreg->emulated_register == reg)
			return nreg;
	}

	/* Try to allocate a non-dirty, non-loaded register.
	 * Loaded registers may be re-used later, so it's better to avoid
	 * re-using one if possible. */
	for (i = 0; i < ARRAY_SIZE(cache->lightrec_regs); i++) {
		struct native_register *nreg = &cache->lightrec_regs[i];
		if (!nreg->used && !nreg->dirty && !nreg->loaded)
			return nreg;
	}

	/* Try to allocate a non-dirty register */
	for (i = 0; i < ARRAY_SIZE(cache->lightrec_regs); i++) {
		struct native_register *nreg = &cache->lightrec_regs[i];
		if (!nreg->used && !nreg->dirty)
			return nreg;
	}

	for (i = 0; i < ARRAY_SIZE(cache->lightrec_regs); i++) {
		struct native_register *nreg = &cache->lightrec_regs[i];
		if (!nreg->used)
			return nreg;
	}

	return NULL;
}

static void lightrec_discard_nreg(struct native_register *nreg)
{
	nreg->loaded = false;
	nreg->output = false;
	nreg->dirty = false;
	nreg->used = false;
	nreg->emulated_register = -1;
}

void lightrec_discard_reg(struct regcache *cache, u8 jit_reg)
{
	lightrec_discard_nreg(lightning_reg_to_lightrec(cache, jit_reg));
}

static void lightrec_unload_nreg(struct regcache *cache, jit_state_t *_jit,
		struct native_register *nreg, u8 jit_reg)
{
	/* If we get a dirty register, store back the old value */
	if (nreg->dirty) {
		s16 offset = offsetof(struct lightrec_state, native_reg_cache)
			+ (nreg->emulated_register << 2);

		jit_stxi_i(offset, LIGHTREC_REG_STATE, jit_reg);
	}

	lightrec_discard_nreg(nreg);
}

void lightrec_unload_reg(struct regcache *cache, jit_state_t *_jit, u8 jit_reg)
{
	lightrec_unload_nreg(cache, _jit,
			lightning_reg_to_lightrec(cache, jit_reg), jit_reg);
}

u8 lightrec_alloc_reg_temp(struct regcache *cache, jit_state_t *_jit)
{
	u8 jit_reg;
	struct native_register *nreg = alloc_temp(cache);
	if (!nreg) {
		/* No free register, no dirty register to free. */
		ERROR("No more registers! Abandon ship!\n");
		return 0;
	}

	jit_reg = lightrec_reg_to_lightning(cache, nreg);
	jit_note(__FILE__, __LINE__);

	lightrec_unload_nreg(cache, _jit, nreg, jit_reg);

	nreg->used = true;
	return jit_reg;
}

u8 lightrec_alloc_reg_out(struct regcache *cache, jit_state_t *_jit, u8 reg)
{
	u8 jit_reg;
	struct native_register *nreg = alloc_in_out(cache, reg);
	if (!nreg) {
		/* No free register, no dirty register to free. */
		ERROR("No more registers! Abandon ship!\n");
		return 0;
	}

	jit_reg = lightrec_reg_to_lightning(cache, nreg);
	jit_note(__FILE__, __LINE__);

	/* If we get a dirty register that doesn't correspond to the one
	 * we're requesting, store back the old value */
	if (nreg->emulated_register != reg)
		lightrec_unload_nreg(cache, _jit, nreg, jit_reg);

	nreg->used = true;
	nreg->output = true;
	nreg->emulated_register = reg;
	return jit_reg;
}

u8 lightrec_alloc_reg_in(struct regcache *cache, jit_state_t *_jit, u8 reg)
{
	u8 jit_reg;
	bool reg_changed;
	struct native_register *nreg = alloc_in_out(cache, reg);
	if (!nreg) {
		/* No free register, no dirty register to free. */
		ERROR("No more registers! Abandon ship!\n");
		return 0;
	}

	jit_reg = lightrec_reg_to_lightning(cache, nreg);
	jit_note(__FILE__, __LINE__);

	/* If we get a dirty register that doesn't correspond to the one
	 * we're requesting, store back the old value */
	reg_changed = nreg->emulated_register != reg;
	if (reg_changed)
		lightrec_unload_nreg(cache, _jit, nreg, jit_reg);

	if (!nreg->loaded && !nreg->dirty && reg != 0) {
		s16 offset = offsetof(struct lightrec_state, native_reg_cache)
			+ (reg << 2);

		/* Load previous value from register cache */
		jit_ldxi_i(jit_reg, LIGHTREC_REG_STATE, offset);
		nreg->loaded = true;
	}

	/* Clear register r0 before use */
	if (reg == 0)
		jit_movi(jit_reg, 0);

	nreg->used = true;
	nreg->output = false;
	nreg->emulated_register = reg;
	return jit_reg;
}

static void free_reg(struct native_register *nreg)
{
	/* Set output registers as dirty */
	if (nreg->used && nreg->output && nreg->emulated_register > 0)
		nreg->dirty = true;
	nreg->used = false;
}

void lightrec_free_reg(struct regcache *cache, u8 jit_reg)
{
	free_reg(lightning_reg_to_lightrec(cache, jit_reg));
}

void lightrec_free_regs(struct regcache *cache)
{
	unsigned int i;

	for (i = 0; i < ARRAY_SIZE(cache->lightrec_regs); i++)
		free_reg(&cache->lightrec_regs[i]);
}

static void clean_reg(jit_state_t *_jit,
		struct native_register *nreg, u8 jit_reg, bool clean)
{
	if (nreg->dirty) {
		s16 offset = offsetof(struct lightrec_state, native_reg_cache)
			+ (nreg->emulated_register << 2);

		jit_stxi_i(offset, LIGHTREC_REG_STATE, jit_reg);
		nreg->loaded |= nreg->dirty;
		nreg->dirty ^= clean;
	}
}

static void clean_regs(struct regcache *cache, jit_state_t *_jit, bool clean)
{
	unsigned int i;

	for (i = 0; i < NUM_REGS; i++)
		clean_reg(_jit, &cache->lightrec_regs[i], JIT_V(i), clean);
	for (i = 0; i < NUM_TEMPS; i++) {
		clean_reg(_jit, &cache->lightrec_regs[i + NUM_REGS],
				JIT_R(i), clean);
	}
}

void lightrec_storeback_regs(struct regcache *cache, jit_state_t *_jit)
{
	clean_regs(cache, _jit, false);
}

void lightrec_clean_regs(struct regcache *cache, jit_state_t *_jit)
{
	clean_regs(cache, _jit, true);
}

void lightrec_clean_reg(struct regcache *cache, jit_state_t *_jit, u8 jit_reg)
{
	struct native_register *reg = lightning_reg_to_lightrec(cache, jit_reg);
	clean_reg(_jit, reg, jit_reg, true);
}

void lightrec_regcache_enter_branch(struct regcache *cache)
{
	memcpy(&cache->lightrec_regs_backup, &cache->lightrec_regs,
			sizeof(cache->lightrec_regs));
}

void lightrec_regcache_leave_branch(struct regcache *cache)
{
	memcpy(&cache->lightrec_regs, &cache->lightrec_regs_backup,
			sizeof(cache->lightrec_regs));
}

void lightrec_regcache_reset(struct regcache *cache)
{
	memset(&cache->lightrec_regs, 0, sizeof(cache->lightrec_regs));
}

struct regcache * lightrec_regcache_init(void)
{
	return calloc(1, sizeof(struct regcache));
}

void lightrec_free_regcache(struct regcache *cache)
{
	free(cache);
}
