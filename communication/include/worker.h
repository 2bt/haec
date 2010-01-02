#pragma once

#include <arpa/inet.h>
#include <unistd.h>


typedef struct {
	int id;
	int parent_id;
	int state;
	double timestamp;

	struct in_addr addr;
	unsigned short port;
	int socket_fd;
	int device_type;

} Worker;


typedef struct {
	int id;
	int parent_id;
	int state;
	double timestamp;

} Switch;


enum {
	WORKER_OFF,
	WORKER_BOOTING,
	WORKER_RUNNING,
	WORKER_HALTING,
	WORKER_ERROR,
};


static inline const char* worker_state_string(int state) {
	static const char* strings[] = {
		"OFF",
		"BOOTING",
		"RUNNING",
		"HALTING",
		"ERROR",
	};
	return strings[state];
}


enum {
	DEVICE_CUBIEBOARD,
	DEVICE_SAMA5D3,

	DEVICE_MAX
};


static inline const char* worker_device_string(int device_type) {
	static const char* strings[] = {
		"CUBIEBOARD",
		"SAMA5D3"
	};
	return strings[device_type];
}


int		worker_init(void);
void	worker_kill(void);
Worker* worker_next(Worker* w);
Worker* worker_find_by_address(struct in_addr a, unsigned short p);
Worker* worker_find_by_socket(int s);
Worker* worker_find_by_id(int id);
