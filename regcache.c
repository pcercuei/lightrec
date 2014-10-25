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
	struct native_register *addr_reg;
	bool used, loaded, dirty, output;
	s8 emulated_register;
};

static struct native_register lightrec_regs[NUM_REGS + NUM_TEMPS];

static inline u8 lightrec_reg_number(const struct native_register *nreg)
{
	return (u8) (((uintptr_t) nreg - (uintptr_t) lightrec_regs)
			/ sizeof(*nreg));
}

static inline u8 lightrec_reg_to_lightning(const struct native_register *nreg)
{
	u8 offset = lightrec_reg_number(nreg);
	return offset < NUM_REGS ? JIT_V(offset) : JIT_R(offset - NUM_REGS);
}

static inline struct native_register * lightning_reg_to_lightrec(u8 reg)
{
	if ((JIT_V0 > JIT_R0 && reg >= JIT_V0) ||
			(JIT_V0 < JIT_R0 && reg < JIT_R0))
		return &lightrec_regs[reg - JIT_V0];
	else
		return &lightrec_regs[NUM_REGS + reg - JIT_R0];
}

static struct native_register * alloc_temp(void)
{
	unsigned int i;

	/* We search the register list in reverse order. As temporaries are
	 * meant to be used only in the emitter functions, they can be mapped to
	 * caller-saved registers, as they won't have to be saved back to
	 * memory. */
	for (i = ARRAY_SIZE(lightrec_regs); i; i--) {
		struct native_register *nreg = &lightrec_regs[i - 1];
		if (!nreg->used)
			return nreg;
	}

	return NULL;
}

static struct native_register * alloc_in_out(u8 reg)
{
	unsigned int i;

	/* Try to find if the register is already mapped somewhere */
	for (i = 0; i < ARRAY_SIZE(lightrec_regs); i++) {
		struct native_register *nreg = &lightrec_regs[i];
		if ((!reg || nreg->loaded || nreg->dirty) &&
				nreg->emulated_register == reg)
			return nreg;
	}

	/* Try to allocate a non-dirty, non-loaded register.
	 * Loaded registers may be re-used later, so it's better to avoid
	 * re-using one if possible. */
	for (i = 0; i < ARRAY_SIZE(lightrec_regs); i++) {
		struct native_register *nreg = &lightrec_regs[i];
		if (!nreg->used && !nreg->dirty && !nreg->loaded)
			return nreg;
	}

	/* Try to allocate a non-dirty register */
	for (i = 0; i < ARRAY_SIZE(lightrec_regs); i++) {
		struct native_register *nreg = &lightrec_regs[i];
		if (!nreg->used && !nreg->dirty)
			return nreg;
	}

	for (i = 0; i < ARRAY_SIZE(lightrec_regs); i++) {
		struct native_register *nreg = &lightrec_regs[i];
		if (!nreg->used)
			return nreg;
	}

	return NULL;
}

static void free_reg(struct native_register *nreg)
{
	/* Set output registers as dirty */
	if (nreg->used && nreg->output && nreg->emulated_register > 0) {
		nreg->dirty = true;
		nreg->addr_reg = NULL;
	}
	nreg->used = false;
}

static void unload_reg(jit_state_t *_jit, u8 jit_reg)
{
	struct native_register *nreg = lightning_reg_to_lightrec(jit_reg);

	/* If we get a dirty register, store back the old value */
	if (nreg->dirty) {
		s16 offset = offsetof(struct lightrec_state, reg_cache)
			+ (nreg->emulated_register << 2);

		jit_stxi_i(offset, LIGHTREC_REG_STATE, jit_reg);
	}

	nreg->addr_reg = NULL;
	nreg->loaded = false;
	nreg->output = false;
	nreg->dirty = false;
	nreg->used = false;
	nreg->emulated_register = -1;
}

u8 lightrec_alloc_reg_temp(jit_state_t *_jit)
{
	u8 jit_reg;
	struct native_register *nreg = alloc_temp();
	if (!nreg) {
		/* No free register, no dirty register to free. */
		ERROR("No more registers! Abandon ship!\n");
		return 0;
	}

	jit_reg = lightrec_reg_to_lightning(nreg);
	jit_note(__FILE__, __LINE__);

	unload_reg(_jit, jit_reg);

	nreg->used = true;
	return jit_reg;
}

u8 lightrec_alloc_reg_out(jit_state_t *_jit, u8 reg)
{
	u8 jit_reg;
	struct native_register *nreg = alloc_in_out(reg);
	if (!nreg) {
		/* No free register, no dirty register to free. */
		ERROR("No more registers! Abandon ship!\n");
		return 0;
	}

	jit_reg = lightrec_reg_to_lightning(nreg);
	jit_note(__FILE__, __LINE__);

	/* If we get a dirty register that doesn't correspond to the one
	 * we're requesting, store back the old value */
	if (nreg->emulated_register != reg)
		unload_reg(_jit, jit_reg);

	nreg->addr_reg = NULL;
	nreg->used = true;
	nreg->output = true;
	nreg->emulated_register = reg;
	return jit_reg;
}

u8 lightrec_alloc_reg_in(jit_state_t *_jit, u8 reg)
{
	u8 jit_reg;
	bool reg_changed;
	struct native_register *nreg = alloc_in_out(reg);
	if (!nreg) {
		/* No free register, no dirty register to free. */
		ERROR("No more registers! Abandon ship!\n");
		return 0;
	}

	jit_reg = lightrec_reg_to_lightning(nreg);
	jit_note(__FILE__, __LINE__);

	/* If we get a dirty register that doesn't correspond to the one
	 * we're requesting, store back the old value */
	reg_changed = nreg->emulated_register != reg;
	if (reg_changed)
		unload_reg(_jit, jit_reg);

	if (!nreg->loaded && !nreg->dirty && reg != 0) {
		s16 offset = offsetof(struct lightrec_state, reg_cache)
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

void lightrec_free_reg(u8 jit_reg)
{
	free_reg(lightning_reg_to_lightrec(jit_reg));
}

void lightrec_free_regs(void)
{
	unsigned int i;

	for (i = 0; i < ARRAY_SIZE(lightrec_regs); i++)
		free_reg(&lightrec_regs[i]);
}

void lightrec_storeback_regs(jit_state_t *_jit)
{
	u8 i;

	jit_note(__FILE__, __LINE__);

	for (i = 0; i < ARRAY_SIZE(lightrec_regs); i++) {
		struct native_register *nreg = &lightrec_regs[i];
		u8 jit_reg = lightrec_reg_to_lightning(nreg);

		if (nreg->dirty) {
			s16 offset = offsetof(struct lightrec_state, reg_cache)
				+ (nreg->emulated_register << 2);

			jit_stxi_i(offset, LIGHTREC_REG_STATE, jit_reg);
		}
	}
}

void lightrec_unlink_addresses(void)
{
	unsigned int i;

	for (i = 0; i < ARRAY_SIZE(lightrec_regs); i++)
		lightrec_regs[i].addr_reg = NULL;
}

void lightrec_regcache_reset(void)
{
	memset(&lightrec_regs, 0, sizeof(lightrec_regs));
}

u8 lightrec_alloc_reg_in_address(jit_state_t *_jit, u8 reg, s16 offset)
{
	u8 addr, rs = lightrec_alloc_reg_in(_jit, reg);
	struct native_register *tmpreg, *nreg = lightning_reg_to_lightrec(rs);

	/* Reuse the previous temp register if it wasn't invalidated */
	if (nreg->addr_reg && nreg->addr_reg->addr_reg == nreg) {
		nreg->addr_reg->used = true;
		return lightrec_reg_to_lightning(nreg->addr_reg);
	}

	jit_movr(JIT_RA0, rs);
	if (offset)
		jit_addi(JIT_RA0, JIT_RA0, offset);

	lightrec_free_regs();

	addr = lightrec_alloc_reg_temp(_jit);
	jit_ldxi(addr, LIGHTREC_REG_STATE,
			offsetof(struct lightrec_state, addr_lookup));
	jit_callr(addr);

	/* The address lookup block returns its value in JIT_RA0
	 * instead of JIT_R0, so that we don't trash a register. */
	jit_movr(addr, JIT_RA0);

	if (offset)
		jit_subi(addr, addr, offset);

	tmpreg = lightning_reg_to_lightrec(addr);

	/* Link the two registers */
	tmpreg->addr_reg = nreg;
	nreg->addr_reg = tmpreg;
	nreg->used = true;

	return addr;
}
