#pragma once

#include "worker.h"

double timestamp(void);

enum {
	EVENT_WORKER_ONLINE,
	EVENT_WORKER_OFFLINE,
	EVENT_WORKER_OFF,
	EVENT_WORK_REQUEST,
	EVENT_WORK_ASSIGN,
	EVENT_WORK_COMPLETE,
};

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

};


Event* queue_append(int type);
Event* queue_pop(void);
