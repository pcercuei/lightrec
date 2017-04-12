/*
 * Copyright (C) 2017 Paul Cercueil <paul@crapouillou.net>
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

#ifndef __RW_CALLBACKS_H__
#define __RW_CALLBACKS_H__

struct lightrec_state;
struct opcode;

void lightrec_lw(struct lightrec_state *state, const struct opcode *op);

#endif /* __RW_CALLBACKS_H__ */
