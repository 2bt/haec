#pragma once


enum {
	STDIN = 0,
	PORT = 1337
};


typedef struct {
	int running;
	int work_counter;
	double timestamp;

	fd_set fds;
	int fdmax;

	FILE* log;
} Server;

extern Server server;

void server_run(void);
void server_log(const char* fmt, ...);
