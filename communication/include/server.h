#pragma once


enum {
	STDIN = 0,
	PORT = 1337
};


struct {
	int running;
	int work_counter;
	double timestamp;

	fd_set fds;
	int fdmax;
} server;



void server_run(void);
