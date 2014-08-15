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

#include "emitter.h"
#include "regcache.h"

#include <lightning.h>

void emit_call_to_interpreter(jit_state_t *_jit, union opcode op)
{
	/* TODO: Generate something... */
}

void rec_special_ADDU(jit_state_t *_jit, union opcode op)
{
	u8 rs, rt, rd;

	jit_name(__func__);

	rs = lightrec_alloc_reg_in(_jit, op.r.rs);
	rt = lightrec_alloc_reg_in(_jit, op.r.rt);
	rd = lightrec_alloc_reg_out(_jit, op.r.rd);

	/* Propagate constants */
	lightrec_rvals[rd].known = lightrec_rvals[rs].known
		&& lightrec_rvals[rt].known;
	if (lightrec_rvals[rd].known)
		lightrec_rvals[rd].value = lightrec_rvals[rs].value +
			lightrec_rvals[rd].value;

	jit_note(__FILE__, __LINE__);
	jit_addr(rd, rs, rt);

	lightrec_free_regs();
}
