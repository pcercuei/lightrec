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

#include "debug.h"
#include "lightrec-private.h"

#include <errno.h>
#include <stdbool.h>
#include <stdlib.h>
#include <sys/queue.h>
#include <threads.h>

struct block_rec {
	struct block *block;
	SLIST_ENTRY(block_rec) next;
};

struct recompiler {
	thrd_t thd;
	cnd_t cond;
	mtx_t mutex;
	bool stop;
	struct block *current_block;
	SLIST_HEAD(block_rec_head, block_rec) list;
};

static void lightrec_compile_list(struct recompiler *rec)
{
	struct block_rec *next;
	struct block *block;
	int ret;

	while (next = SLIST_FIRST(&rec->list)) {
		block = next->block;
		rec->current_block = block;

		mtx_unlock(&rec->mutex);

		ret = lightrec_compile_block(block);
		if (ret) {
			ERROR("Unable to compile block at PC 0x%x: %d\n",
			      block->pc, ret);
		}

		mtx_lock(&rec->mutex);

		SLIST_REMOVE(&rec->list, next, block_rec, next);
		free(next);
		cnd_signal(&rec->cond);
	}

	rec->current_block = NULL;
}

static int lightrec_recompiler_thd(void *d)
{
	struct recompiler *rec = d;

	mtx_lock(&rec->mutex);

	for (;;) {
		do {
			cnd_wait(&rec->cond, &rec->mutex);

			if (rec->stop) {
				mtx_unlock(&rec->mutex);
				return 0;
			}

		} while (SLIST_EMPTY(&rec->list));

		lightrec_compile_list(rec);
	}
}

struct recompiler *lightrec_recompiler_init(void)
{
	struct recompiler *rec;
	int ret;

	rec = malloc(sizeof(*rec));
	if (!rec) {
		ERROR("Cannot create recompiler: Out of memory\n");
		return NULL;
	}

	rec->stop = false;
	rec->current_block = NULL;
	SLIST_INIT(&rec->list);

	ret = cnd_init(&rec->cond);
	if (ret) {
		ERROR("Cannot init cond variable: %d\n", ret);
		goto err_free_rec;
	}

	ret = mtx_init(&rec->mutex, mtx_plain);
	if (ret) {
		ERROR("Cannot init mutex variable: %d\n", ret);
		goto err_cnd_destroy;
	}

	ret = thrd_create(&rec->thd, lightrec_recompiler_thd, rec);
	if (ret) {
		ERROR("Cannot create recompiler thread: %d\n", ret);
		goto err_mtx_destroy;
	}

	return rec;

err_mtx_destroy:
	mtx_destroy(&rec->mutex);
err_cnd_destroy:
	cnd_destroy(&rec->cond);
err_free_rec:
	free(rec);
	return NULL;
}

void lightrec_free_recompiler(struct recompiler *rec)
{
	rec->stop = true;

	/* Stop the thread */
	mtx_lock(&rec->mutex);
	cnd_signal(&rec->cond);
	mtx_unlock(&rec->mutex);
	thrd_join(rec->thd, NULL);

	mtx_destroy(&rec->mutex);
	cnd_destroy(&rec->cond);
	free(rec);
}

int lightrec_recompiler_add(struct recompiler *rec, struct block *block)
{
	struct block_rec *block_rec;

	mtx_lock(&rec->mutex);

	SLIST_FOREACH(block_rec, &rec->list, next) {
		if (block_rec->block == block) {
			mtx_unlock(&rec->mutex);
			return 0;
		}
	}

	block_rec = malloc(sizeof(*block_rec));
	if (!block_rec) {
		mtx_unlock(&rec->mutex);
		return -ENOMEM;
	}

	DEBUG("Adding block PC 0x%x to recompiler\n", block->pc);

	block_rec->block = block;
	SLIST_INSERT_HEAD(&rec->list, block_rec, next);

	/* Signal the thread */
	cnd_signal(&rec->cond);
	mtx_unlock(&rec->mutex);

	return 0;
}

void lightrec_recompiler_remove(struct recompiler *rec, struct block *block)
{
	struct block_rec *block_rec;

	mtx_lock(&rec->mutex);

	SLIST_FOREACH(block_rec, &rec->list, next) {
		if (block_rec->block == block) {
			if (block == rec->current_block) {
				/* Block is being recompiled - wait for
				 * completion */
				do {
					cnd_wait(&rec->cond, &rec->mutex);
				} while (block == rec->current_block);
			} else {
				/* Block is not yet being processed - remove it
				 * from the list */
				SLIST_REMOVE(&rec->list, block_rec,
					     block_rec, next);
				free(block_rec);
			}

			break;
		}
	}

	mtx_unlock(&rec->mutex);
}
