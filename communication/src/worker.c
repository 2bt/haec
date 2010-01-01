#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <racr/racr.h>

#include "event.h"
#include "worker.h"



static Worker* workers = NULL;
static int worker_count;


void worker_kill(void) {
	if (workers) {
		free(workers);
		workers = NULL;
	}
	worker_count = 0;
}


int worker_init(void) {
	worker_kill();

	FILE* f = fopen("config.txt", "r");
	if (!f) return -1;

	double time = timestamp();
	char line[256];
	char addr[256];
	char type[256];
	int id;
	int parent_id;

	while (fgets(line, sizeof(line), f)) {
		if (line[0] == '#') continue;
		if (strlen(line) == strspn(line, " \t\n")) continue;

		char* endl = strchr(line, '\n');
		if (*endl) *endl = '\0';

		if (sscanf(line, "worker %d %d %s %s",
					&id, &parent_id, addr, type) == 4) {
			worker_count++;
			workers = realloc(workers, sizeof(Worker) * worker_count);
			Worker* w = &workers[worker_count - 1];

			inet_pton(AF_INET, addr, &w->addr);
			w->is_switch = 0;
			w->id = id;
			w->parent_id = parent_id;

			w->port = 0;
			w->socket_fd = -1;
			w->state = WORKER_OFF;
			w->timestamp = time;

			int t;
			for (t = 0; t < DEVICE_MAX; t++) {
				if (strcmp(type, worker_device_string(t)) == 0) {
					w->device_type = t;
					break;
				}
			}
			if (t == DEVICE_MAX) {
				fclose(f);
				return -2;
			}

			racr_call_str("add-worker-to-ast", "iisd", id, parent_id, type, time);
		}
		else if (sscanf(line, "switch %d %d", &id, &parent_id) == 2) {

			worker_count++;
			workers = realloc(workers, sizeof(Worker) * worker_count);
			Worker* w = &workers[worker_count - 1];
			memset(w, 0, sizeof(Worker));

			w->is_switch = 1;
			w->id = id;
			w->parent_id = parent_id;
			w->state = WORKER_OFF;
			w->timestamp = time;

			racr_call_str("add-switch-to-ast", "iid", id, parent_id, time);
		}
		else {
			fclose(f);
			return -3;
		}
	}
	fclose(f);
	return 0;
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
		if (w->is_switch) continue;
		if (w->addr.s_addr == a.s_addr && w->port == p) return w;
	}
	return NULL;
}


Worker* worker_find_by_socket(int s) {
	int i;
	for (i = 0; i < worker_count; i++) {
		Worker* w = &workers[i];
		if (w->is_switch) continue;
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
