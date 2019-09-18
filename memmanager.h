/*
 * Copyright (C) 2019 Paul Cercueil <paul@crapouillou.net>
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

#ifndef __MEMMANAGER_H__
#define __MEMMANAGER_H__

#include "lightrec.h"

void * lightrec_malloc(enum mem_type type, unsigned int len);
void * lightrec_calloc(enum mem_type type, unsigned int len);
void lightrec_free(enum mem_type type, unsigned int len, void *ptr);

void lightrec_register(enum mem_type type, unsigned int len);
void lightrec_unregister(enum mem_type type, unsigned int len);

#endif /* __MEMMANAGER_H__ */
