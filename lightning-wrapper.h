/* SPDX-License-Identifier: LGPL-2.1-or-later */
/*
 * Copyright (C) 2022 Paul Cercueil <paul@crapouillou.net>
 */

#ifndef __LIGHTNING_WRAPPER_H__
#define __LIGHTNING_WRAPPER_H__

#include <lightning.h>

#include "lightrec-config.h"

#if __WORDSIZE == 32

#define jit_ldxi_ui(u,v,w)	jit_ldxi_i(u,v,w)
#define jit_ldr_ui(u,v)		jit_ldr_i(u,v)
#define jit_extr_i(u,v)		jit_movr(u,v)
#define jit_extr_ui(u,v)	jit_movr(u,v)
#define jit_retval_ui(u)	jit_retval(u)
#define jit_getarg_ui(u,v)	jit_getarg_i(u,v)

#endif

#define jit_b()			jit_beqr(0, 0)

#define jit_add_state(u,v)	jit_addr(u,v,LIGHTREC_REG_STATE)

#if OPT_SH4_USE_GBR
#undef jit_stxi_c
#undef jit_stxi_s
#undef jit_stxi_i
#define jit_stxi_c(offt,addr,reg) \
	jit_new_node_www(jit_code_stxi_c,(offt),\
			 (addr) == LIGHTREC_REG_STATE && (offt) <= 255 ? _GBR : (addr),(reg))
#define jit_stxi_s(offt,addr,reg) \
	jit_new_node_www(jit_code_stxi_s,(offt),\
			 (addr) == LIGHTREC_REG_STATE && (offt) <= 510 ? _GBR : (addr),(reg))
#define jit_stxi_i(offt,addr,reg) \
	jit_new_node_www(jit_code_stxi_i,(offt),\
			 (addr) == LIGHTREC_REG_STATE && (offt) > 60 && (offt) <= 1020 ? _GBR : (addr),(reg))
#endif


#endif /* __LIGHTNING_WRAPPER_H__ */
