// SPDX-License-Identifier: LGPL-2.1-or-later
/*
 * Copyright (C) 2026 Paul Cercueil <paul@crapouillou.net>
 */

#include "gte.h"
#include "regcache.h"

void rec_gte_NCLIP(struct lightrec_cstate *cstate,
		   const struct block *block, u16 offset)
{
	struct regcache *reg_cache = cstate->reg_cache;
	jit_state_t *_jit = block->_jit;
	u8 sx0, sx1, sx2, sy0, sy1, sy2;

	_jit_name(_jit, __func__);
	jit_note(__FILE__, __LINE__);

	sx2 = lightrec_alloc_reg_temp(reg_cache, _jit);
	jit_addi(sx2, LIGHTREC_REG_STATE, cp2d_i_offset(12));

	/* Load SX0 / SY0 */
	sx0 = lightrec_alloc_reg_temp(reg_cache, _jit);
	jit_ldxai_i(sx0, sx2, 4);

	/* Load SX1 / SY1 */
	sx1 = lightrec_alloc_reg_temp(reg_cache, _jit);
	jit_ldxai_i(sx1, sx2, 4);

	/* Load SX2 / SY2 */
	jit_ldr(sx2, sx2);

	sy0 = lightrec_alloc_reg_temp(reg_cache, _jit);
	jit_rshi(sy0, sx0, 16);
	jit_extr_s(sx0, sx0);

	sy1 = lightrec_alloc_reg_temp(reg_cache, _jit);
	jit_rshi(sy1, sx1, 16);
	jit_extr_s(sx1, sx1);

	sy2 = lightrec_alloc_reg_temp(reg_cache, _jit);
	jit_rshi(sy2, sx2, 16);
	jit_extr_s(sx2, sx2);

	jit_subr(sy1, sy1, sy2);
	jit_mulr(sx0, sx0, sy1);
	jit_addr(sy1, sy1, sy2);

	jit_subr(sy2, sy2, sy0);
	jit_mulr(sx1, sx1, sy2);

	jit_subr(sy0, sy0, sy1);
	jit_mulr(sx2, sx2, sy0);

	jit_addr(sx0, sx0, sx1);
	jit_addr(sx0, sx0, sx2);

	jit_stxi_i(cp2d_i_offset(24), LIGHTREC_REG_STATE, sx0);

	lightrec_free_reg(reg_cache, sx0);
	lightrec_free_reg(reg_cache, sx1);
	lightrec_free_reg(reg_cache, sx2);
	lightrec_free_reg(reg_cache, sy0);
	lightrec_free_reg(reg_cache, sy1);
	lightrec_free_reg(reg_cache, sy2);
}
