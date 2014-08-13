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
#include <stdbool.h>

extern u32 register_cache[32];

struct native_register {
	bool used, loaded, dirty, output;
	u8 emulated_register;
};

static struct native_register lightrec_regs[JIT_V_NUM + JIT_R_NUM];

static inline u8 lightrec_reg_number(struct native_register *nreg)
{
	return (u8) (((uintptr_t) nreg - (uintptr_t) lightrec_regs)
			/ sizeof(*nreg));
}

static inline u8 lightrec_reg_to_lightning(struct native_register *nreg)
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

	/* Try to allocate a non-dirty register first */
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

static struct native_register * alloc_in_out(u8 reg)
{
	unsigned int i;

	/* Try to find if the register is already mapped somewhere */
	for (i = 0; i < ARRAY_SIZE(lightrec_regs); i++) {
		struct native_register *nreg = &lightrec_regs[i];
		if (!nreg->used && (nreg->loaded || nreg->dirty) &&
				nreg->emulated_register == reg)
			return nreg;
	}

	return alloc_temp();
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

	/* If we get a dirty register, store back the old value */
	if (nreg->dirty) {
		jit_str_i(&register_cache[nreg->emulated_register], jit_reg);
		nreg->dirty = false;
	}

	nreg->output = false;
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

	/* If we get a dirty register that doesn't correspond to the one
	 * we're requesting, store back the old value */
	if (nreg->dirty && nreg->emulated_register != reg) {
		jit_str_i(&register_cache[nreg->emulated_register], jit_reg);
		nreg->dirty = false;
	}

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

	/* If we get a dirty register that doesn't correspond to the one
	 * we're requesting, store back the old value */
	if (nreg->dirty && nreg->emulated_register != reg) {
		jit_str_i(&register_cache[nreg->emulated_register], jit_reg);
		nreg->dirty = false;
		nreg->loaded = false;
	}

	if (!nreg->loaded && !nreg->dirty) {
		nreg->loaded = true;

		/* Load previous value from register cache */
		jit_ldr_i(jit_reg, &register_cache[reg]);
	}

	nreg->used = true;
	nreg->output = false;
	nreg->emulated_register = reg;
	return jit_reg;
}

void lightrec_free_reg(jit_state_t *_jit, u8 jit_reg)
{
	struct native_register *nreg = lightning_reg_to_lightrec(jit_reg);
	nreg->used = false;

	/* Set output registers as dirty */
	if (nreg->output)
		nreg->dirty = true;
}

void lightrec_storeback_regs(jit_state_t *_jit)
{
	unsigned int i;

	for (i = 0; i < ARRAY_SIZE(lightrec_regs); i++) {
		struct native_register *nreg = &lightrec_regs[i];

		if (nreg->used)
			WARNING("Found a used register when storing back!\n");

		if (nreg->dirty) {
			u8 jit_reg = lightrec_reg_to_lightning(nreg);
			jit_str_i(&register_cache[nreg->emulated_register],
					jit_reg);
		}

		nreg->loaded = false;
		nreg->dirty = false;
		nreg->used = false;
	}
}
