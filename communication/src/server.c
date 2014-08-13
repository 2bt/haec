#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <error.h>
#include <arpa/inet.h>
#include <unistd.h>

#include <racr/racr.h>

#include "server.h"
#include "worker.h"
#include "event.h"
#include "cambri.h"



void eval_string(const char* str);


ssize_t sendf(int s, const char* format, ...) {
	char line[256];
	va_list args;
	va_start(args, format);
	vsnprintf(line, sizeof(line), format, args);
	va_end(args);
	return send(s, line, strlen(line) + 1, 0);
}


static void server_command() {

	char msg[1024];
	fgets(msg, sizeof(msg), stdin);
	msg[strlen(msg) - 1] = '\0';

	int id, size, time, threads, work_id;
	Worker* w;

	if (strcmp(msg, "exit") == 0) {
		server.running = 0;
		printf("exiting...\n");
	}
	else if (strcmp(msg, "status") == 0) {
		printf(" id   | switch | address:port          | socket | state   | since \n");
		printf("------+--------+-----------------------+--------+---------+-------\n");
		double time = timestamp();
		for (w = worker_next(NULL); w; w = worker_next(w)) {
			char s[INET_ADDRSTRLEN];
			inet_ntop(AF_INET, &w->addr, s, sizeof(s));
			printf(" %-4d | %-6d | %-15s:%05d | %6d | %-7s | %5.2f\n",
				w->id, w->switch_id, s, w->port, w->socket_fd,
				worker_state_string(w), time - w->timestamp);
		}
	}
	else if (sscanf(msg, "work %d %d", &size, &time) == 2) {
		Event* e = event_append(EVENT_WORK_REQUEST);
		e->work_id = server.work_counter++;
		e->load_size = size;
		e->time_due = time;
	}


	// testing...
	else if (msg[0] == '(') eval_string(msg);

	else if (sscanf(msg, "boot %d", &id) == 1) {
		w = worker_find_by_id(id);
		if (!w) {
			printf("error: %s\n", msg);
			return;
		}
		if (w->state != WORKER_OFF) {
			printf("error: worker %d is not OFF\n", w->id);
			return;
		}
		w->state = WORKER_BOOTING;
		w->timestamp = timestamp();
		cambri_write("set_profiles %d 4", w->id);
		cambri_write("mode c %d 4", w->id);

	}

	else if (sscanf(msg, "halt %d", &id) == 1) {
		w = worker_find_by_id(id);
		if (!w) {
			printf("error: %s\n", msg);
			return;
		}
		if (w->state != WORKER_IDLE) {
			printf("error: worker %d is not IDLE\n", w->id);
			return;
		}
		sendf(w->socket_fd, "halt");
		w->state = WORKER_HALTING;
		w->timestamp = timestamp();
	}

	else if (sscanf(msg, "work-command %d %d %d %d", &id, &work_id, &threads, &size) == 4) {
		w = worker_find_by_id(id);
		if (!w) {
			printf("error: %s\n", msg);
			return;
		}
		Event* e = event_append(EVENT_WORK_COMMAND);
		e->worker = w;
		e->work_id = work_id;
		e->load_size = size;
		e->threads = threads;

	}

	else {
		// TESTING
		int id = atoi(msg);
		char* s = strchr(msg, ' ');
		if (!s) {
			printf("error: %s\n", msg);
			return;
		}
		s++;
		w = worker_find_by_id(id);
		if (!w) {
			printf("error: %s\n", msg);
			return;
		}
		sendf(w->socket_fd, s);
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

		Event* e = event_append(EVENT_WORKER_ONLINE);
		e->worker = w;

	}
}


static void server_receive(int s) {
	char msg[1024];
	ssize_t len = recv(s, msg, sizeof(msg), 0);

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

		Event* e = event_append(EVENT_WORKER_OFFLINE);
		e->worker = w;
		return;
	}

	//printf("received %d bytes from worker %d: %.*s\n", (int) len, w->id, (int) len, msg);

	char* p = strchr(msg, ' ');
	if (p) *p++ = '\0';
	int ack;

	if (strcmp(msg, "work-complete") == 0) {
		int id;
		if (sscanf(p, "%d %d", &id, &ack) != 2) goto ERROR;
		Event* e = event_append(EVENT_WORK_COMPLETE);
		e->worker = w;
		e->work_id = id;
		e->ack = ack;
	}
	else if (strcmp(msg, "work-ack") == 0) {
		if (sscanf(p, "%d", &ack) != 1) goto ERROR;
		Event* e = event_append(EVENT_WORK_ACK);
		e->worker = w;
		e->ack = ack;
	}
	else if (strcmp(msg, "halt-ack") == 0) {
		if (sscanf(p, "%d", &ack) != 1) goto ERROR;
		Event* e = event_append(EVENT_HALT_ACK);
		e->worker = w;
		e->ack = ack;
	}
	else if (strcmp(msg, "mem-ack") == 0) {
		if (sscanf(p, "%d", &ack) != 1) goto ERROR;
		Event* e = event_append(EVENT_MEM_ACK);
		e->worker = w;
		e->ack = ack;
	}
	else if (strcmp(msg, "cpu-ack") == 0) {
	}
	else goto ERROR;
	return;
ERROR:
	error(1, 0, "server_receive");
}


void server_process_events(void) {
	Event* e;
	double time = timestamp();
	while ((e = event_pop())) {

		printf("%5.2f event %s\n", time - server.timestamp, event_type_string(e));


		Worker* w = e->worker;
		switch (e->type) {
		case EVENT_WORKER_ONLINE: {
				if (w->state == WORKER_BOOTING) {
					printf("worker %d booted successfully in %5.2f seconds\n", w->id, time - w->timestamp);
				}
				w->state = WORKER_IDLE;
				w->timestamp = time;
				racr_call_str("event-worker-online", "id", w->id, time);

				char s[INET_ADDRSTRLEN];
				inet_ntop(AF_INET, &w->addr, s, sizeof(s));
				printf("worker %d connected from %s:%d on socket %d\n", w->id, s, w->port, w->socket_fd);
			}
			break;

		case EVENT_WORKER_OFFLINE:
			if (w->state != WORKER_HALTING) {
				printf("worker %d hung up unexpectedly\n", w->id);
				w->state = WORKER_ERROR;
				w->timestamp = time;
			}
			racr_call_str("event-worker-off", "id", w->id, time);
			break;

		case EVENT_WORKER_OFF:
			w->state = WORKER_OFF;
			w->timestamp = time;
			w->socket_fd = -1;
			w->port = 0;

			// cambri power shutdown
			cambri_write("mode o %d", w->id);

			racr_call_str("event-worker-off", "id", w->id, time);
			break;

		case EVENT_WORK_REQUEST:
			racr_call_str("event-work-request", "diid", time, e->work_id, e->load_size, e->time_due);
			break;

		case EVENT_WORK_COMMAND:
			sendf(w->socket_fd, "work %d %d %d", e->work_id, e->threads, e->load_size);
			break;

		case EVENT_WORK_ACK:
			break;

		case EVENT_WORK_COMPLETE:
			racr_call_str("event-work-complete", "idi", w->id, time, e->work_id);
			break;

		case EVENT_MEM_COMMAND:
			sendf(w->socket_fd, "mem");
			break;

		case EVENT_MEM_ACK:
			break;

		case EVENT_HALT_COMMAND:
			sendf(w->socket_fd, "halt");
			break;

		case EVENT_HALT_ACK:
			break;

		default:
			printf("unknown event: %d\n", e->type);
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
	server.work_counter = 0;


	server.timestamp = timestamp();


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
				Event* e = event_append(EVENT_WORKER_OFF);
				e->worker = w;
			}
		}


		server_process_events();
	}

	close(listener);

	cambri_kill();
	worker_kill();
}

