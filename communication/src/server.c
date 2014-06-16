#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <error.h>
#include <arpa/inet.h>
#include <unistd.h>

#include "server.h"


enum { PORT = 1337 };


enum {
	STATE_OFF,
	STATE_BOOTING,
	STATE_IDLE,
	STATE_WORKING,
	STATE_HALTING
};

const char* state_strings[] = {
	"OFF",
	"BOOTING",
	"IDLE",
	"WORKING",
	"HALTING"
};


typedef struct {
	int cambri_port;
	struct in_addr addr;

	unsigned short port;
	int socket_fd;

	int state;
	// TODO: timestamp of last state change

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


enum { WORKER_COUNT = 3 };


Worker workers[WORKER_COUNT];


static void init_workers(void) {
	static const char* a[WORKER_COUNT] = {
		"127.0.0.1",
		"127.0.0.1",
		"127.0.0.1"
	};
	int i;
	for (i = 0; i < WORKER_COUNT; i++) {
		Worker* w = &workers[i];
		w->cambri_port = i + 1;
		inet_pton(AF_INET, a[i], &w->addr);
		w->port = 0;
		w->socket_fd = -1;
		w->state = STATE_OFF;
	}
}


Worker* get_worker_by_address(struct in_addr a, unsigned short p) {
	int i;
	for (i = 0; i < WORKER_COUNT; i++) {
		Worker* w = &workers[i];
		if (w->addr.s_addr == a.s_addr && w->port == p) return w;
	}
	return NULL;
}


Worker* get_worker_by_socket(int s) {
	int i;
	for (i = 0; i < WORKER_COUNT; i++) {
		Worker* w = &workers[i];
		if (w->socket_fd == s) return w;
	}
	return NULL;
}






enum { STDIN = 0 };

struct {
	fd_set fds;
	int fdmax;

} server;


static void server_new_connection(int s) {
	struct sockaddr_in client;
	socklen_t size = sizeof(client);
	int newfd = accept(s, (struct sockaddr*) &client, &size);
	if (newfd < 0) perror("accept");
	else {

		// check for worker ip address
		Worker* w = get_worker_by_address(client.sin_addr, 0);
		if (!w) {
			char s[INET_ADDRSTRLEN];
			inet_ntop(AF_INET, &client.sin_addr, s, sizeof(s));
			printf("unexpected connection from %s:%d\n", s, client.sin_port);
			close(newfd);
		}
		else {
			FD_SET(newfd, &server.fds);
			if (newfd > server.fdmax) server.fdmax = newfd;

			w->port = client.sin_port;
			w->socket_fd = newfd;
			w->state = STATE_IDLE;

			char s[INET_ADDRSTRLEN];
			inet_ntop(AF_INET, &w->addr, s, sizeof(s));
			printf("new connection from %s:%d on socket %d\n", s, w->port, w->socket_fd);
		}
	}
}

static void server_receive(int s) {
	Worker* w = get_worker_by_socket(s);

	char msg[1024];
	size_t len = recv(s, msg, sizeof(msg), 0);
	if (len <= 0) {
		close(s);
		FD_CLR(s, &server.fds);
		printf("socket %d hung up\n", s);
		if (w) {
			w->socket_fd = -1;
			w->port = 0;
			w->state = STATE_HALTING;
		}
		return;
	}


	printf("received %d bytes from socket %d: %.*s\n", (int) len, s, (int) len, msg);
}


void server_run(void) {

	int listener = socket(AF_INET, SOCK_STREAM, 0);
	if (listener < 0) error(1, 0, "socket");
	int yes = 1;
	if (setsockopt(listener, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int)) < 0) {
		error(1, 0, "setsockopt");
	}
	struct sockaddr_in addr = { AF_INET, htons(PORT), { INADDR_ANY } };
	if (bind(listener, (struct sockaddr*)&addr, sizeof(addr)) < 0) {
		error(1, 0, "bind");
	}
	listen(listener, 3);

	FD_ZERO(&server.fds);
	FD_SET(STDIN, &server.fds);
	FD_SET(listener, &server.fds);
	server.fdmax = listener;


	init_workers();

	printf("entering event loop.\n");
	int running = 1;
	while (running) {
		fd_set fds = server.fds;
		struct timeval timeout = { 0, 100000 };
		int count = select(server.fdmax + 1, &fds, NULL, NULL, &timeout);
		if (count < 0) error(1, 0, "select");
		if (count == 0) continue;

		int i;
		for (i = 0; i <= server.fdmax; i++) {
			if (FD_ISSET(i, &fds)) {


				if (i == STDIN) {
					// command from stdin

					char msg[1024];
					fgets(msg, sizeof(msg), stdin);
					msg[strlen(msg) - 1] = '\0';
					printf("stdin: %s\n", msg);


					if (strcmp(msg, "exit") == 0) {
						running = 0;
						printf("exiting...\n");
					}
					if (strcmp(msg, "status") == 0) {
						int i;
						for (i = 0; i < WORKER_COUNT; i++) {
							Worker* w = &workers[i];
							char s[INET_ADDRSTRLEN];
							inet_ntop(AF_INET, &w->addr, s, sizeof(s));
							printf("%d %s:%d %d %s\n",
								w->cambri_port, s, w->port, w->socket_fd, state_strings[w->state]);
						}
					}
					else {
						// TESTING
						send(server.fdmax, msg, strlen(msg) + 1, 0);
					}

				}
				else if (i == listener) server_new_connection(listener);
				else server_receive(i);
			}
		}
	}

	close(listener);
}

