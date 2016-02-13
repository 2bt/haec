#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include "event.h"
#include "server.h"
#include "sim.h"




double timestamp(void) {
	return absolute_timestamp() - server.timestamp;
}


const char* format_timestamp(double t) {
	t = ((int) (t * 100)) * 0.01;
	double s = fmod(t, 60);
	int m = (int)(t / 60);
	int h = m / 60 % 99;
	m %= 60;
	static char buf[12];
	snprintf(buf, sizeof(buf), "%02d:%02d:%05.2f", h, m, s);
	return buf;
}


double read_timestamp(const char* t) {
	int h, m;
	double s;
	if (sscanf(t, "%d:%d:%lf", &h, &m, &s) != 3) return -1;
	return s + m * 60 + h * 3600;
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
	server_log_event("%s %s", format_timestamp(time), event_type_string(e->type));
	switch (e->type) {
	case EVENT_SCENARIO_START:
		server_log_event(" (file: %s)", e->scenario);
		break;
	case EVENT_SWITCH_ON:
	case EVENT_SWITCH_OFF:
	case EVENT_WORKER_ON:
	case EVENT_WORKER_ONLINE:
	case EVENT_WORKER_OFFLINE:
	case EVENT_WORKER_OFF:
	case EVENT_WORKER_REBOOT:
	case EVENT_HALT_COMMAND:
	case EVENT_MEM_COMMAND:
		server_log_event(" (id: %d)", e->worker->id);
		break;
	case EVENT_MEM_ACK:
	case EVENT_WORK_ACK:
	case EVENT_HALT_ACK:
		server_log_event(" (id: %d; ack: %d)", e->worker->id, e->ack);
		break;
	case EVENT_WORK_REQUEST:
		server_log_event(" (work-id: %d; load-size: %.3lf; deadline: %.2lf)", e->work_id, e->load_size, e->deadline);
		break;
	case EVENT_WORK_COMMAND:
		server_log_event(" (id: %d; work-id: %d; load-size: %.3lf)", e->worker->id, e->work_id, e->load_size);
		break;
	case EVENT_WORK_COMPLETE:
		server_log_event(" (id: %d; work-id: %d; ack: %d)", e->worker->id, e->work_id, e->ack);
		break;
	case EVENT_SCENARIO_DONE:
	case EVENT_E_METER_RESET:
	default: break;
	}
	server_log_event("\n");
}


