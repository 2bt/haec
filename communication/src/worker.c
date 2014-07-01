#include "event.h"
#include "worker.h"



Worker workers[WORKER_COUNT];


void Worker_initialize(void) {
	static const char* a[WORKER_COUNT] = {
		//"192.168.1.42",
		"127.0.0.1",
		"127.0.0.1",
		"127.0.0.1"
	};
	double time = timestamp();
	int i;
	for (i = 0; i < WORKER_COUNT; i++) {
		Worker* w = &workers[i];
		w->cambri_port = i + 1;
		inet_pton(AF_INET, a[i], &w->addr);
		w->port = 0;
		w->socket_fd = -1;
		w->state = WORKER_OFF;
		w->timestamp = time;
	}
}


Worker* Worker_find_by_address(struct in_addr a, unsigned short p) {
	int i;
	for (i = 0; i < WORKER_COUNT; i++) {
		Worker* w = &workers[i];
		if (w->addr.s_addr == a.s_addr && w->port == p) return w;
	}
	return NULL;
}


Worker* Worker_find_by_socket(int s) {
	int i;
	for (i = 0; i < WORKER_COUNT; i++) {
		Worker* w = &workers[i];
		if (w->socket_fd == s) return w;
	}
	return NULL;
}


Worker* Worker_find_by_cambri_port(int i) {
	if (i < 1 || i > WORKER_COUNT) return NULL;
	return &workers[i - 1];
}

const char* Worker_get_state_string(const Worker* w) {
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

