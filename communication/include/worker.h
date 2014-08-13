#pragma once

#include <arpa/inet.h>
#include <unistd.h>


#define TIME_BOOTING	54.7 // WORKER_BOOTING -> WORKER_IDLE
#define TIME_HALTING	12.1 // WORKER_HALTING -> WORKER_OFF


typedef struct {
	struct in_addr addr;
	int id;
	int switch_id;

	unsigned short port;
	int socket_fd;

	int state;
	double timestamp;	// timestamp of the last state change

/*
	// config
	int cpus;
	int freq;
	int map_threads;
	int free_memory;

	// queue
	int work_id;
	int input_len;
*/
} Worker;


typedef struct {
	int id;
} Switch;



enum {
	WORKER_OFF,
	WORKER_BOOTING,
	WORKER_IDLE,
	WORKER_WORKING,
	WORKER_HALTING,
	WORKER_ERROR,
};


static inline const char* worker_state_string(const Worker* w) {
	static const char* strings[] = {
		"OFF",
		"BOOTING",
		"IDLE",
		"WORKING",
		"HALTING",
		"ERROR",
	};
	return strings[w->state];
}



void worker_init(void);
void worker_kill(void);
Worker* worker_next(Worker* w);
Worker* worker_find_by_address(struct in_addr a, unsigned short p);
Worker* worker_find_by_socket(int s);
Worker* worker_find_by_id(int id);
