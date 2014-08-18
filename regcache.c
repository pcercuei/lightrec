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

extern u32 register_cache[32];

struct native_register {
	bool used, loaded, dirty, output;
	u8 emulated_register;
};

struct register_value lightrec_rvals[JIT_V_NUM + JIT_R_NUM];

static struct native_register lightrec_regs[JIT_V_NUM + JIT_R_NUM];

static inline u8 lightrec_reg_number(const struct native_register *nreg)
{
	return (u8) (((uintptr_t) nreg - (uintptr_t) lightrec_regs)
			/ sizeof(*nreg));
}

static inline u8 lightrec_reg_to_lightning(const struct native_register *nreg)
{
	u8 offset = lightrec_reg_number(nreg);
	return offset < JIT_V_NUM ? JIT_V(offset) : JIT_R(offset - JIT_V_NUM);
}

static inline struct native_register * lightning_reg_to_lightrec(u8 reg)
{
	if ((jit_v(0) > jit_r(0)) ^ (reg > jit_r(0)))
		return &lightrec_regs[JIT_V_NUM + reg - jit_r(0)];
	else
		return &lightrec_regs[reg - jit_v(0)];
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
		if ((nreg->loaded || nreg->dirty) &&
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
	if (nreg->used && nreg->output)
		nreg->dirty = true;
	nreg->used = false;
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

	/* If we get a dirty register, store back the old value */
	if (nreg->dirty) {
		jit_sti_i(&register_cache[nreg->emulated_register], jit_reg);
		nreg->dirty = false;
	}

	lightrec_rvals[jit_reg].known = false;
	nreg->output = false;
	nreg->used = true;
	return jit_reg;
}

u8 lightrec_alloc_reg_temp_with_value(jit_state_t *_jit, u32 value)
{
	unsigned int i;
	u8 jit_reg;

	/* Try to find a temp register that already contains the value */
	for (i = ARRAY_SIZE(lightrec_rvals); i; i--) {
		if (lightrec_rvals[i - 1].known &&
				lightrec_rvals[i - 1].value == value) {
			struct native_register *nreg =
				lightning_reg_to_lightrec(i - 1);

			if (!nreg->used && !nreg->dirty) {
				nreg->output = false;
				nreg->used = true;
				return i - 1;
			}
		}
	}

	/* If not found, alloc a new temp register, and load the value */
	jit_reg = lightrec_alloc_reg_temp(_jit);
	lightrec_rvals[jit_reg].known = true;
	lightrec_rvals[jit_reg].value = value;
	jit_movi(jit_reg, value);
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
	if (nreg->dirty && nreg->emulated_register != reg) {
		u32 addr = (u32) &register_cache[nreg->emulated_register];
		u8 jit_tmp = lightrec_alloc_reg_temp_with_value(_jit,
				addr & 0xffff0000);

		jit_stxi_i(addr & 0xffff, jit_tmp, jit_reg);
		free_reg(lightning_reg_to_lightrec(jit_tmp));

		nreg->dirty = false;
	}

	lightrec_rvals[jit_reg].known = false;
	nreg->used = true;
	nreg->output = true;
	nreg->emulated_register = reg;
	return jit_reg;
}

u8 lightrec_alloc_reg_in(jit_state_t *_jit, u8 reg)
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
	if (nreg->dirty && nreg->emulated_register != reg) {
		u32 addr = (u32) &register_cache[nreg->emulated_register];
		u8 jit_tmp = lightrec_alloc_reg_temp_with_value(_jit,
				addr & 0xffff0000);

		jit_stxi_i(addr & 0xffff, jit_tmp, jit_reg);
		nreg->dirty = false;
		nreg->loaded = false;

		free_reg(lightning_reg_to_lightrec(jit_tmp));
	}

	if (!nreg->loaded && !nreg->dirty) {
		u32 addr = (u32) &register_cache[reg];
		u8 jit_tmp = lightrec_alloc_reg_temp_with_value(_jit,
				addr & 0xffff0000);

		/* Load previous value from register cache */
		jit_ldxi_i(jit_reg, jit_tmp, addr & 0xffff);
		nreg->loaded = true;

		free_reg(lightning_reg_to_lightrec(jit_tmp));
	}

	lightrec_rvals[jit_reg].known = false;
	nreg->used = true;
	nreg->output = false;
	nreg->emulated_register = reg;
	return jit_reg;
}

void lightrec_free_regs(void)
{
	unsigned int i;

	for (i = 0; i < ARRAY_SIZE(lightrec_regs); i++)
		free_reg(&lightrec_regs[i]);
}

static void storeback_regs(jit_state_t *_jit, u8 start, u8 end)
{
	u8 i;

	jit_note(__FILE__, __LINE__);

	for (i = start; i < end; i++) {
		struct native_register *nreg = &lightrec_regs[i];
		u8 jit_reg = lightrec_reg_to_lightning(nreg);

		if (nreg->used)
			WARNING("Found a used register when storing back!\n");

		if (nreg->dirty) {
			u32 addr = (u32)
				&register_cache[nreg->emulated_register];
			u8 jit_tmp = lightrec_alloc_reg_temp_with_value(_jit,
					addr & 0xffff0000);

			jit_stxi_i(addr & 0xffff, jit_tmp, jit_reg);
			free_reg(lightning_reg_to_lightrec(jit_tmp));
		}

		nreg->loaded = false;
		nreg->dirty = false;
		nreg->used = false;
		lightrec_rvals[jit_reg].known = false;
	}
}

void lightrec_storeback_regs(jit_state_t *_jit)
{
	storeback_regs(_jit, JIT_V_NUM, ARRAY_SIZE(lightrec_regs));
}

void lightrec_storeback_all_regs(jit_state_t *_jit)
{
	storeback_regs(_jit, 0, ARRAY_SIZE(lightrec_regs));
}

void lightrec_regcache_reset(void)
{
	memset(&lightrec_regs, 0, sizeof(lightrec_regs));
	memset(&lightrec_rvals, 0, sizeof(lightrec_rvals));
}
