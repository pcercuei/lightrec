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

static void rec_gte_clamp_u16(struct regcache *reg_cache,
			     jit_state_t *_jit, u8 reg)
{
	u8 tmp1, tmp2;

	tmp1 = lightrec_alloc_reg_temp(reg_cache, _jit);
	tmp2 = lightrec_alloc_reg_temp(reg_cache, _jit);

	/* Clamp value to 0 .. 0xffff */
	jit_gei(tmp1, reg, 0);
	jit_movzr(reg, tmp1, tmp1);
	jit_movi(tmp1, 0xffff);
	jit_gtr(tmp2, reg, tmp1);
	jit_movnr(reg, tmp1, tmp2);

	lightrec_free_reg(reg_cache, tmp1);
	lightrec_free_reg(reg_cache, tmp2);
}

static void rec_gte_calc_OTZ(struct regcache *reg_cache,
			     jit_state_t *_jit, u8 mac0)
{
	jit_rshi(mac0, mac0, 12);
	rec_gte_clamp_u16(reg_cache, _jit, mac0);

	/* Store gteOTZ */
	jit_stxi_s(cp2d_sl_offset(7), LIGHTREC_REG_STATE, mac0);
}

void rec_gte_AVSZ3(struct lightrec_cstate *cstate,
		   const struct block *block, u16 offset)
{
	struct regcache *reg_cache = cstate->reg_cache;
	jit_state_t *_jit = block->_jit;
	u8 sz1, sz2, sz3, zsf3;

	_jit_name(_jit, __func__);
	jit_note(__FILE__, __LINE__);

	sz3 = lightrec_alloc_reg_temp(reg_cache, _jit);
	jit_addi(sz3, LIGHTREC_REG_STATE, cp2d_i_offset(17));

	/* Load gteSZ1 */
	sz1 = lightrec_alloc_reg_temp(reg_cache, _jit);
	jit_ldxai_i(sz1, sz3, 4);

	/* Load gteSZ2 */
	sz2 = lightrec_alloc_reg_temp(reg_cache, _jit);
	jit_ldxai_i(sz2, sz3, 4);

	jit_extr_us(sz1, sz1);

	/* Load gteSZ3 */
	jit_ldr(sz3, sz3);

	jit_extr_us(sz2, sz2);

	/* Load gteZSF3 */
	zsf3 = lightrec_alloc_reg_temp(reg_cache, _jit);
	jit_ldxi_s(zsf3, LIGHTREC_REG_STATE, cp2c_sl_offset(29));

	jit_addr(sz1, sz1, sz2);

	jit_extr_us(sz3, sz3);
	jit_addr(sz1, sz1, sz3);

	jit_mulr(sz1, sz1, zsf3);

	/* Store MAC0 */
	jit_stxi_i(cp2d_i_offset(24), LIGHTREC_REG_STATE, sz1);

	lightrec_free_reg(reg_cache, sz2);
	lightrec_free_reg(reg_cache, sz3);
	lightrec_free_reg(reg_cache, zsf3);

	rec_gte_calc_OTZ(reg_cache, _jit, sz1);

	lightrec_free_reg(reg_cache, sz1);
}

void rec_gte_AVSZ4(struct lightrec_cstate *cstate,
		   const struct block *block, u16 offset)
{
	struct regcache *reg_cache = cstate->reg_cache;
	jit_state_t *_jit = block->_jit;
	u8 sz0, sz1, sz2, sz3, zsf4;

	_jit_name(_jit, __func__);
	jit_note(__FILE__, __LINE__);

	sz3 = lightrec_alloc_reg_temp(reg_cache, _jit);
	jit_addi(sz3, LIGHTREC_REG_STATE, cp2d_i_offset(16));

	/* Load gteSZ0 */
	sz0 = lightrec_alloc_reg_temp(reg_cache, _jit);
	jit_ldxai_i(sz0, sz3, 4);

	/* Load gteSZ1 */
	sz1 = lightrec_alloc_reg_temp(reg_cache, _jit);
	jit_ldxai_i(sz1, sz3, 4);

	jit_extr_us(sz0, sz0);

	/* Load gteSZ2 */
	sz2 = lightrec_alloc_reg_temp(reg_cache, _jit);
	jit_ldxai_i(sz2, sz3, 4);

	jit_extr_us(sz1, sz1);

	/* Load gteSZ3 */
	jit_ldr(sz3, sz3);

	jit_extr_us(sz2, sz2);

	/* Load gteZSF4 */
	zsf4 = lightrec_alloc_reg_temp(reg_cache, _jit);
	jit_ldxi_s(zsf4, LIGHTREC_REG_STATE, cp2c_sl_offset(30));

	jit_addr(sz0, sz0, sz1);
	jit_addr(sz0, sz0, sz2);

	jit_extr_us(sz3, sz3);
	jit_addr(sz1, sz1, sz3);

	jit_mulr(sz0, sz0, zsf4);

	/* Store MAC0 */
	jit_stxi_i(cp2d_i_offset(24), LIGHTREC_REG_STATE, sz0);

	lightrec_free_reg(reg_cache, sz1);
	lightrec_free_reg(reg_cache, sz2);
	lightrec_free_reg(reg_cache, sz3);
	lightrec_free_reg(reg_cache, zsf4);

	rec_gte_calc_OTZ(reg_cache, _jit, sz0);

	lightrec_free_reg(reg_cache, sz0);
}
