#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <error.h>
#include <arpa/inet.h>
#include <time.h>
#include <unistd.h>

#include "server.h"
#include "cambri.h"


enum { PORT = 1337 };

// time in seconds
const double TIME_BOOTING = 54.7; // STATE_BOOTING -> STATE_IDLE
const double TIME_HALTING = 12.1; // STATE_HALTING -> STATE_OFF


enum {
	STATE_OFF,
	STATE_BOOTING,
	STATE_IDLE,
	STATE_WORKING,
	STATE_HALTING,
	STATE_ERROR,
};

const char* state_strings[] = {
	"OFF",
	"BOOTING",
	"IDLE",
	"WORKING",
	"HALTING",
	"ERROR",
};


double timestamp(void) {
	struct timespec t;
	clock_gettime(CLOCK_REALTIME, &t);
	return t.tv_sec + t.tv_nsec * 1e-9;
}


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


enum { WORKER_COUNT = 3 };


Worker workers[WORKER_COUNT];


static void init_workers(void) {
	static const char* a[WORKER_COUNT] = {
		"192.168.1.42",
		"127.0.0.1",
		"127.0.0.1"
	};
	double time = timestamp();
	int i;
	for (i = 0; i < WORKER_COUNT; i++) {
		Worker* w = &workers[i];
		w->cambri_port = i + 1;
		inet_pton(AF_INET, a[i], &w->addr);
		w->port = 0;
		w->socket_fd = -1;
		w->state = STATE_OFF;
		w->timestamp = time;
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


Worker* get_worker_by_cambri_port(int i) {
	if (i < 1 || i > WORKER_COUNT) return NULL;
	return &workers[i - 1];
}




struct {
	fd_set fds;
	int fdmax;
	int running;
} server;


static void server_command() {

	char msg[1024];
	fgets(msg, sizeof(msg), stdin);
	msg[strlen(msg) - 1] = '\0';

	int cambri_port;

	if (strcmp(msg, "exit") == 0) {
		server.running = 0;
		printf("exiting...\n");
	}

	else if (strcmp(msg, "status") == 0) {
		printf(" cambri port | address:port          | socket | state   | since \n");
		printf("-------------+-----------------------+--------+---------+-------\n");
		int i;
		for (i = 0; i < WORKER_COUNT; i++) {
			double time = timestamp();
			Worker* w = &workers[i];
			char s[INET_ADDRSTRLEN];
			inet_ntop(AF_INET, &w->addr, s, sizeof(s));
			printf("%12d | %-15s:%05d | %6d | %-7s | %5.2f\n",
				w->cambri_port, s, w->port, w->socket_fd,
				state_strings[w->state], time - w->timestamp);
		}
	}

	else if (sscanf(msg, "boot %d", &cambri_port) == 1) {
		Worker* w = get_worker_by_cambri_port(cambri_port);
		if (!w) {
			printf("error: %s\n", msg);
			return;
		}
		if (w->state != STATE_OFF) {
			printf("error: worker %d is not OFF\n", w->cambri_port);
			return;
		}
		w->state = STATE_BOOTING;
		w->timestamp = timestamp();
		cambri_write("set_profiles %d 4", w->cambri_port);
		cambri_write("mode c %d 4", w->cambri_port);

	}

	else if (sscanf(msg, "halt %d", &cambri_port) == 1) {
		Worker* w = get_worker_by_cambri_port(cambri_port);
		if (!w) {
			printf("error: %s\n", msg);
			return;
		}
		if (w->state != STATE_IDLE) {
			printf("error: worker %d is not IDLE\n", w->cambri_port);
			return;
		}
		send(w->socket_fd, "halt", 5, 0);
		w->state = STATE_HALTING;
		w->timestamp = timestamp();
	}

	else {
		// TESTING
		int cambri_port = atoi(msg);
		char* s = strchr(msg, ' ');
		if (!s) {
			printf("error: %s\n", msg);
			return;
		}
		s++;
		Worker* w = get_worker_by_cambri_port(cambri_port);
		if (!w) {
			printf("error: %s\n", msg);
			return;
		}
		send(w->socket_fd, s, strlen(s) + 1, 0);
	}
}


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

			double time = timestamp();
			if (w->state == STATE_BOOTING) {
				printf("worker %d booted successfully in %5.2f seconds\n", w->cambri_port, time - w->timestamp);
			}

			w->state = STATE_IDLE;
			w->timestamp = time;
			w->port = client.sin_port;
			w->socket_fd = newfd;

			char s[INET_ADDRSTRLEN];
			inet_ntop(AF_INET, &w->addr, s, sizeof(s));
			printf("worker %d connected from %s:%d on socket %d\n", w->cambri_port, s, w->port, w->socket_fd);
		}
	}
}


static void server_receive(int s) {
	Worker* w = get_worker_by_socket(s);
	if (!w) printf("unknown receiver socker: %d\n", s);

	char msg[1024];
	size_t len = recv(s, msg, sizeof(msg), 0);
	if (len <= 0) {
		close(s);
		FD_CLR(s, &server.fds);
		if (w) {
			if (w->state != STATE_HALTING) {
				printf("worker %d hung up unexpectedly\n", w->cambri_port);
				w->state = STATE_ERROR;
			}
			w->socket_fd = -1;
			w->port = 0;
		}
		return;
	}

	if (w) {
		printf("received %d bytes from worker %d: %.*s\n", (int) len, w->cambri_port, (int) len, msg);
	}
}


enum { STDIN = 0 };

void server_run(void) {

	cambri_init();

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
	server.running = 1;


	init_workers();

	printf("entering server loop.\n");
	while (server.running) {
		fd_set fds = server.fds;
		struct timeval timeout = { 0, 100000 };
		int count = select(server.fdmax + 1, &fds, NULL, NULL, &timeout);
		if (count < 0) error(1, 0, "select");
		if (count == 0) continue;

		int i;
		for (i = 0; i <= server.fdmax; i++) {
			if (FD_ISSET(i, &fds)) {
				if (i == STDIN) server_command();
				else if (i == listener) server_new_connection(listener);
				else server_receive(i);
			}
		}

		double time = timestamp();
		for (i = 0; i < WORKER_COUNT; i++) {
			Worker* w = &workers[i];

			if (w->state == STATE_HALTING
			&& time - w->timestamp > TIME_HALTING) {
				w->state = STATE_OFF;
				w->timestamp = timestamp();
				w->socket_fd = -1;
				w->port = 0;

				// cambri power shutdown
				cambri_write("mode o %d", w->cambri_port);

			}
		}
	}

	close(listener);

	cambri_kill();
}

