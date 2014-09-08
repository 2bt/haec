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

	FILE* log_fd;
	FILE* run_fd;
	double run_timestamp;
} Server;

extern Server server;

void server_run(int argc, char** argv);
void server_log(const char* fmt, ...);
