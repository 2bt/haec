#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <error.h>
#include <arpa/inet.h>
#include <unistd.h>

#include "server.h"
#include "worker.h"
#include "event.h"
#include "cambri.h"





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
		printf(" cambri | switch | address:port          | socket | state   | since \n");
		printf("--------+--------+-----------------------+--------+---------+-------\n");
		double time = timestamp();
		Worker* w;
		for (w = worker_next(NULL); w; w = worker_next(w)) {
			char s[INET_ADDRSTRLEN];
			inet_ntop(AF_INET, &w->addr, s, sizeof(s));
			printf("%7d | %6d | %-15s:%05d | %6d | %-7s | %5.2f\n",
				w->cambri_port, w->switch_id, s, w->port, w->socket_fd,
				worker_state_string(w), time - w->timestamp);
		}
	}

	else if (sscanf(msg, "boot %d", &cambri_port) == 1) {
		Worker* w = worker_find_by_cambri_port(cambri_port);
		if (!w) {
			printf("error: %s\n", msg);
			return;
		}
		if (w->state != WORKER_OFF) {
			printf("error: worker %d is not OFF\n", w->cambri_port);
			return;
		}
		w->state = WORKER_BOOTING;
		w->timestamp = timestamp();
		cambri_write("set_profiles %d 4", w->cambri_port);
		cambri_write("mode c %d 4", w->cambri_port);

	}

	else if (sscanf(msg, "halt %d", &cambri_port) == 1) {
		Worker* w = worker_find_by_cambri_port(cambri_port);
		if (!w) {
			printf("error: %s\n", msg);
			return;
		}
		if (w->state != WORKER_IDLE) {
			printf("error: worker %d is not IDLE\n", w->cambri_port);
			return;
		}
		send(w->socket_fd, "halt", 5, 0);
		w->state = WORKER_HALTING;
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
		Worker* w = worker_find_by_cambri_port(cambri_port);
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
		Worker* w = worker_find_by_address(client.sin_addr, 0);
		if (!w) {
			char s[INET_ADDRSTRLEN];
			inet_ntop(AF_INET, &client.sin_addr, s, sizeof(s));
			printf("unexpected connection from %s:%d\n", s, client.sin_port);
			close(newfd);
			return;
		}

		FD_SET(newfd, &server.fds);
		if (newfd > server.fdmax) server.fdmax = newfd;

		w->port = client.sin_port;
		w->socket_fd = newfd;

		Event* e = queue_append(EVENT_WORKER_ONLINE);
		e->worker = w;

	}
}


static void server_receive(int s) {
	char msg[1024];
	size_t len = recv(s, msg, sizeof(msg), 0);

	Worker* w = worker_find_by_socket(s);
	if (!w) {
		printf("unknown receiver socker: %d\n", s);
		return;
	}

	if (len <= 0) {
		close(s);
		FD_CLR(s, &server.fds);

		w->socket_fd = -1;
		w->port = 0;

		Event* e = queue_append(EVENT_WORKER_OFFLINE);
		e->worker = w;
		return;
	}

	printf("received %d bytes from worker %d: %.*s\n", (int) len, w->cambri_port, (int) len, msg);
}




void server_event_loop(void) {
	Event* e;
	while ((e = queue_pop())) {
		printf("Event %d\n", e->type);

		switch (e->type) {
		case EVENT_WORKER_ONLINE: {
				double time = timestamp();
				Worker* w = e->worker;
				if (w->state == WORKER_BOOTING) {
					printf("worker %d booted successfully in %5.2f seconds\n", w->cambri_port, time - w->timestamp);
				}
				w->state = WORKER_IDLE;
				w->timestamp = time;

				char s[INET_ADDRSTRLEN];
				inet_ntop(AF_INET, &w->addr, s, sizeof(s));
				printf("worker %d connected from %s:%d on socket %d\n", w->cambri_port, s, w->port, w->socket_fd);
			}
			break;

		case EVENT_WORKER_OFFLINE:
			if (e->worker->state != WORKER_HALTING) {
				printf("worker %d hung up unexpectedly\n", e->worker->cambri_port);
				e->worker->state = WORKER_ERROR;
				e->worker->timestamp = timestamp();
			}
			break;

		case EVENT_WORKER_OFF:
			e->worker->state = WORKER_OFF;
			e->worker->timestamp = timestamp();
			e->worker->socket_fd = -1;
			e->worker->port = 0;

			// cambri power shutdown
			cambri_write("mode o %d", e->worker->cambri_port);
			break;


		default:
			break;
		}
		free(e);
	}

}



void server_run(void) {

	//cambri_init();
	worker_init();



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
		Worker* w;
		for (w = worker_next(NULL); w; w = worker_next(w)) {
			if (w->state == WORKER_HALTING
			&& time - w->timestamp > TIME_HALTING) {
				Event* e = queue_append(EVENT_WORKER_OFF);
				e->worker = w;
			}
		}


		server_event_loop();
	}

	close(listener);

	cambri_kill();
	worker_kill();
}

