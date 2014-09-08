#include <stdlib.h>
#include <stdio.h>
#include <time.h>

#include "event.h"
#include "server.h"



double timestamp(void) {
	struct timespec t;
	clock_gettime(CLOCK_REALTIME, &t);
	return t.tv_sec + t.tv_nsec * 1e-9;
}


typedef struct {
	Event* first;
	Event* last;
} Queue;


static Queue queue = { NULL, NULL };


Event* event_append(int type) {
	Event*e = calloc(1, sizeof(Event));
	e->type = type;
	if (queue.last) {
		queue.last->next = e;
		queue.last = e;
	}
	else {
		queue.first = e;
		queue.last = e;
	}
	return e;
}


Event* event_pop(void) {
	Event* e = queue.first;
	if (queue.first == queue.last) {
		queue.first = NULL;
		queue.last = NULL;
	}
	else queue.first = queue.first->next;
	if (e) e->next = NULL;
	return e;
}


void event_print(const Event* e, double time) {
	server_log("%8.2f event %s", time - server.timestamp, event_type_string(e));
	switch (e->type) {
	case EVENT_WORKER_ONLINE:
	case EVENT_WORKER_OFFLINE:
	case EVENT_WORKER_OFF:
	case EVENT_HALT_COMMAND:
	case EVENT_MEM_COMMAND:
		server_log(" (id: %d)", e->worker->id);
		break;
	case EVENT_MEM_ACK:
	case EVENT_WORK_ACK:
	case EVENT_HALT_ACK:
		server_log(" (id: %d; ack: %d)", e->worker->id, e->ack);
		break;
	case EVENT_WORK_REQUEST:
		server_log(" (work-id: %d; load-size: %d; time-due: %.2f)", e->work_id, e->load_size, e->time_due);
		break;
	case EVENT_WORK_COMMAND:
		server_log(" (work-id: %d; threads: %d; load-size: %d)", e->work_id, e->threads, e->load_size);
		break;
	case EVENT_WORK_COMPLETE:
		server_log(" (id: %d; work-id: %d; ack: %d)", e->worker->id, e->work_id, e->ack);
		break;
	default: break;
	}
	server_log("\n");
}


