#include <stdlib.h>
#include <stdio.h>
#include <error.h>
#include <string.h>

#include <racr/racr.h>

#include "event.h"
#include "worker.h"



static Worker* workers = NULL;
static int worker_count;

static Switch* switchs = NULL;
static int switch_count;


void worker_kill(void) {
	if (workers) {
		free(workers);
		workers = NULL;
	}
	worker_count = 0;
	if (switchs) {
		free(switchs);
		switchs = NULL;
	}
	switch_count = 0;
}


void worker_init(void) {
	worker_kill();

	FILE* f = fopen("config.txt", "r");
	if (!f) error(1, 0, "config.txt missing\n");

	double time = timestamp();
	char line[256];
	char addr[256];
	int id;
	int switch_id;

	while (fgets(line, sizeof(line), f)) {
		if (line[0] == '#') continue;
		if (strlen(line) == strspn(line, " \t\n")) continue;

		char* endl = strchr(line, '\n');
		if (*endl) *endl = '\0';

		if (sscanf(line, "worker %s %d %d",
					addr, &id, &switch_id) == 3) {
			worker_count++;
			workers = realloc(workers, sizeof(Worker) * worker_count);
			Worker* w = &workers[worker_count - 1];

			inet_pton(AF_INET, addr, &w->addr);
			w->id = id;
			w->switch_id = switch_id;

			w->port = 0;
			w->socket_fd = -1;
			w->state = WORKER_OFF;
			w->timestamp = time;

			racr_call_str("add-worker-to-ast", "idi", id, time, switch_id);

			continue;
		}
		// TODO: switch

		printf("error reading config: %s\n", line);
	}
	fclose(f);

}


Worker* worker_next(Worker* w) {
	if (!w) return workers;
	if (w == workers + worker_count - 1) return NULL;
	return w + 1;
}


Worker* worker_find_by_address(struct in_addr a, unsigned short p) {
	int i;
	for (i = 0; i < worker_count; i++) {
		Worker* w = &workers[i];
		if (w->addr.s_addr == a.s_addr && w->port == p) return w;
	}
	return NULL;
}


Worker* worker_find_by_socket(int s) {
	int i;
	for (i = 0; i < worker_count; i++) {
		Worker* w = &workers[i];
		if (w->socket_fd == s) return w;
	}
	return NULL;
}


Worker* worker_find_by_id(int id) {
	int i;
	for (i = 0; i < worker_count; i++) {
		Worker* w = &workers[i];
		if (w->id == id) return w;
	}
	return NULL;
}


const char* worker_state_string(const Worker* w) {
	static const char* state_strings[] = {
		"OFF",
		"BOOTING",
		"IDLE",
		"WORKING",
		"HALTING",
		"ERROR",
	};
	return state_strings[w->state];
}

