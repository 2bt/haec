#pragma once

#include "worker.h"


typedef struct Event Event;
struct Event {
	Event*	next;
	int		type;

	// validity of fields depends on type
	Worker* worker;
	int		load_size;
	double	time_due;
	int		work_id;
	int		threads;
	int		ack;
};


enum {
	EVENT_WORKER_ON,
	EVENT_WORKER_ONLINE,
	EVENT_WORKER_OFFLINE,
	EVENT_WORKER_OFF,

	EVENT_WORK_REQUEST,
	EVENT_WORK_COMMAND,
	EVENT_WORK_ACK,
	EVENT_WORK_COMPLETE,

	EVENT_HALT_COMMAND,
	EVENT_HALT_ACK,

	EVENT_MEM_COMMAND,
	EVENT_MEM_ACK,
};


static inline const char* event_type_string(const Event* e) {
	static const char* strings[] = {
		"WORKER_ON",
		"WORKER_ONLINE",
		"WORKER_OFFLINE",
		"WORKER_OFF",

		"WORK_REQUEST",
		"WORK_COMMAND",
		"WORK_ACK",
		"WORK_COMPLETE",

		"HALT_COMMAND",
		"HALT_ACK",

		"MEM_COMMAND",
		"MEM_ACK",
	};
	return strings[e->type];
}


Event*	event_append(int type);
Event*	event_pop(void);
void	event_print(const Event* e, double time);

double	timestamp(void);
const char* format_timestamp(double t);
double read_timestamp(const char* t);
