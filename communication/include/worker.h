#pragma once

#include <arpa/inet.h>
#include <unistd.h>


#define TIME_BOOTING	54.7 // WORKER_BOOTING -> WORKER_IDLE
#define TIME_HALTING	12.1 // WORKER_HALTING -> WORKER_OFF

enum {
	WORKER_OFF,
	WORKER_BOOTING,
	WORKER_IDLE,
	WORKER_WORKING,
	WORKER_HALTING,
	WORKER_ERROR,


	WORKER_COUNT = 3
};


typedef struct {
	int cambri_port;
	struct in_addr addr;

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


void Worker_initialize(void);
Worker* Worker_find_by_address(struct in_addr a, unsigned short p);
Worker* Worker_find_by_socket(int s);
Worker* Worker_find_by_cambri_port(int i);
const char* Worker_get_state_string(const Worker* w);

