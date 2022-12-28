/* SPDX-License-Identifier: LGPL-2.1-or-later */
/*
 * Copyright (C) 2022 Paul Cercueil <paul@crapouillou.net>
 */

#ifndef __LIGHTREC_CONSTPROP_H__
#define __LIGHTREC_CONSTPROP_H__

#include "lightrec.h"

#define LIGHTREC_CONSTPROP_INITIALIZER { { 0, 0xffffffff, 0 }, }

struct opcode;

struct constprop_data {
	u32 value;
	u32 known;
	u32 sign;
};

static inline _Bool is_known(const struct constprop_data *v, u8 reg)
{
	return v[reg].known == 0xffffffff;
}

void lightrec_consts_propagate(const struct opcode *op,
			       const struct opcode *prev,
			       struct constprop_data *v);

#endif /* __LIGHTREC_CONSTPROP_H__ */
