/* SPDX-License-Identifier: LGPL-2.1-or-later */
/*
 * Copyright (C) 2026 Paul Cercueil <paul@crapouillou.net>
 */

#ifndef __LIGHTREC_GTE_H__
#define __LIGHTREC_GTE_H__

#include "lightrec-private.h"

void rec_gte_NCLIP(struct lightrec_cstate *cstate,
		   const struct block *block, u16 offset);
void rec_gte_AVSZ3(struct lightrec_cstate *cstate,
		   const struct block *block, u16 offset);
void rec_gte_AVSZ4(struct lightrec_cstate *cstate,
		   const struct block *block, u16 offset);

#endif /* __LIGHTREC_GTE_H__ */
