/* SPDX-License-Identifier: LGPL-2.1-or-later */
/*
 * Copyright (C) 2014-2021 Paul Cercueil <paul@crapouillou.net>
 */

#ifndef __EMITTER_H__
#define __EMITTER_H__

#include "lightrec.h"

struct block;
struct opcode;

void lightrec_rec_opcode(struct lightrec_state *state, const struct block *block,
			 u16 offset);
void lightrec_emit_eob(struct lightrec_state *state, const struct block *block,
			 u16 offset, _Bool after_op);

#endif /* __EMITTER_H__ */
