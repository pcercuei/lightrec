/*
 * Copyright (C) 2018 Paul Cercueil <paul@crapouillou.net>
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
#include "recompiler.h"
#include "lightrec-private.h"

#include <pthread.h>
#include <stdlib.h>
#include <sys/queue.h>

struct block_entry {
	u32 pc;
	STAILQ_ENTRY(block_entry) next;
};

STAILQ_HEAD(block_entry_head, block_entry);

struct recompiler {
	struct lightrec_state *state;
	struct block_entry_head todo;

	pthread_t thd;
	pthread_cond_t has_job;
	pthread_mutex_t has_job_lock;

	bool exit_thread;
};

static void * recompiler_thd(void *d)
{
	struct recompiler *rec = d;
	struct block_entry *entry;
	struct block *block;
	u32 pc;

	DEBUG("Starting recompiler thread\n");

	pthread_mutex_lock(&rec->has_job_lock);

	for (;;) {
		for (;;) {
			if (!STAILQ_EMPTY(&rec->todo))
				break;

			if (rec->exit_thread)
				break;

			DEBUG("THS: Waiting for tasks...\n");
			pthread_cond_wait(&rec->has_job, &rec->has_job_lock);
		}

		pthread_mutex_unlock(&rec->has_job_lock);
		if (rec->exit_thread)
			return NULL;

		entry = STAILQ_FIRST(&rec->todo);

		pc = entry->pc;

		DEBUG("THD: recompiling block at PC 0x%08x\n", pc);
		block = lightrec_recompile_block(rec->state, pc);
		if (!block)
			ERROR("Unable to recompile block at PC 0x%x\n", pc);
		else
			lightrec_register_block(rec->state->block_cache, block);

		DEBUG("THD: Done, locking mutex and remove head\n");
		pthread_mutex_lock(&rec->has_job_lock);
		STAILQ_REMOVE_HEAD(&rec->todo, next);
		free(entry);
	}
}

void lightrec_request_recompile(struct recompiler *rec, u32 pc)
{
	struct block_entry *entry;

	pthread_mutex_lock(&rec->has_job_lock);
	STAILQ_FOREACH(entry, &rec->todo, next) {
		if (entry->pc == pc) {
			/* No need to recompile if it's already pending */
			goto out_unlock;
		}
	}

	entry = malloc(sizeof(*entry));
	if (!entry) {
		/* if malloc returns NULL, fail silently; Lightrec will call us
		 * again the next time it finds the same block of code. */
		goto out_unlock;
	}

	DEBUG("Request to recompile 0x%08x\n", pc);
	entry->pc = pc;

	STAILQ_INSERT_TAIL(&rec->todo, entry, next);
	pthread_cond_signal(&rec->has_job);

out_unlock:
	pthread_mutex_unlock(&rec->has_job_lock);
}

struct recompiler * lightrec_recompiler_init(struct lightrec_state *state)
{
	struct recompiler *rec = malloc(sizeof(*rec));
	int ret;

	if (!rec)
		return NULL;

	rec->state = state;
	rec->exit_thread = false;

	pthread_cond_init(&rec->has_job, NULL);
	pthread_mutex_init(&rec->has_job_lock, NULL);
	STAILQ_INIT(&rec->todo);

	ret = pthread_create(&rec->thd, NULL, recompiler_thd, rec);
	if (ret) {
		ERROR("Unable to create thread: %i\n", ret);
		pthread_cond_destroy(&rec->has_job);
		pthread_mutex_destroy(&rec->has_job_lock);
		return NULL;
	}

	return rec;
}

void lightrec_free_recompiler(struct recompiler *rec)
{
	pthread_mutex_lock(&rec->has_job_lock);
	rec->exit_thread = true;
	pthread_cond_signal(&rec->has_job);
	pthread_mutex_unlock(&rec->has_job_lock);

	pthread_join(rec->thd, NULL);
	pthread_cond_destroy(&rec->has_job);
	pthread_mutex_destroy(&rec->has_job_lock);
	free(rec);
}
