#include <stdlib.h>
#include <time.h>


#include "event.h"

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


Event* queue_append(int type) {
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

Event* queue_pop(void) {
	Event* e = queue.first;
	if (queue.first == queue.last) {
		queue.first = NULL;
		queue.last = NULL;
	}
	else queue.first = queue.first->next;
	if (e) e->next = NULL;
	return e;
}

