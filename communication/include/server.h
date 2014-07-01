#pragma once


enum {
	STDIN = 0,
	PORT = 1337
};


struct {
	fd_set fds;
	int fdmax;
	int running;
} server;



void server_run(void);
