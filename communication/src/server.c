#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>
#include <error.h>

#include <racr/racr.h>

#include "server.h"
#include "worker.h"
#include "event.h"
#include "cambri.h"


// TODO: move this to scheme
#define TIME_HALTING 12.1

Server server;


void eval_string(const char* str);


ssize_t sendf(int s, const char* format, ...) {
	char line[256];
	va_list args;
	va_start(args, format);
	vsnprintf(line, sizeof(line), format, args);
	va_end(args);
	return send(s, line, strlen(line) + 1, 0);
}


void server_log(const char* fmt, ...) {
	char buf[256];
	va_list args;
	va_start(args, fmt);
	vsprintf(buf, fmt, args);
	va_end(args);
	printf("%s", buf);
	fprintf(server.log_fd, "%s", buf);
	fflush(server.log_fd);
}


static void server_command(char* cmd) {

	int id, size, time, threads, work_id;
	Worker* w;
	char scenario[256];

	if (strcmp(cmd, "exit") == 0) {
		server.running = 0;
		printf("exiting...\n");
	}
	else if (strcmp(cmd, "status") == 0) {
		printf(" id   | switch | address:port          | socket | state   | time\n");
		printf("------+--------+-----------------------+--------+---------+-------------\n");
		double time = timestamp();
		for (w = worker_next(NULL); w; w = worker_next(w)) {
			char s[INET_ADDRSTRLEN];
			inet_ntop(AF_INET, &w->addr, s, sizeof(s));
			printf(" %-4d | %-6d | %-15s:%05d | %6d | %-7s | %s\n",
				w->id, w->switch_id, s, w->port, w->socket_fd,
				worker_state_string(w->state), format_timestamp(time - w->timestamp));
		}
	}
	else if (sscanf(cmd, "work %d %d", &size, &time) == 2) {
		Event* e = event_append(EVENT_WORK_REQUEST);
		e->work_id = server.work_counter++;
		e->load_size = size;
		e->time_due = time;
	}
	else if (sscanf(cmd, "scenario %s", scenario) == 1) {
		Event* e = event_append(EVENT_SCENARIO_START);
		e->scenario = strdup(scenario);
	}
	else if (strcmp(cmd, "done") == 0) {
		fclose(server.scenario_fd);
		server.scenario_fd = NULL;
		event_append(EVENT_SCENARIO_DONE);
	}


	// testing...
	else if (cmd[0] == '(') eval_string(cmd);

	else if (strncmp(cmd, "cambri0 ", 7) == 0) {
		cambri_write(0, cmd + 7);
		char buf[4096] = {};
		int ret = cambri_read(0, buf, sizeof(buf));
		if (ret > 0) printf("%s\n", buf);
	}
	else if (strncmp(cmd, "cambri1 ", 7) == 0) {
		cambri_write(1, cmd + 7);
		char buf[4096] = {};
		int ret = cambri_read(1, buf, sizeof(buf));
		if (ret > 0) printf("%s\n", buf);
	}

	else if (sscanf(cmd, "boot %d", &id) == 1) {
		w = worker_find_by_id(id);
		if (!w) {
			printf("error: %s\n", cmd);
			return;
		}
		if (w->state != WORKER_OFF) {
			printf("error: worker %d is not OFF\n", w->id);
			return;
		}
		w->state = WORKER_BOOTING;
		w->timestamp = timestamp();
		cambri_set_mode(w->id, CAMBRI_CHARGE);
	}

	else if (sscanf(cmd, "halt %d", &id) == 1) {
		w = worker_find_by_id(id);
		if (!w) {
			printf("error: %s\n", cmd);
			return;
		}
		sendf(w->socket_fd, "halt");
		w->state = WORKER_HALTING;
		w->timestamp = timestamp();
	}

	else if (sscanf(cmd, "work-command %d %d %d %d", &id, &work_id, &threads, &size) == 4) {
		w = worker_find_by_id(id);
		if (!w) {
			printf("error: %s\n", cmd);
			return;
		}
		Event* e = event_append(EVENT_WORK_COMMAND);
		e->worker = w;
		e->work_id = work_id;
		e->load_size = size;
		e->threads = threads;

	}

	else {
		printf("error: %s\n", cmd);
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
	char msg[256];
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


static int server_read_scenario_command() {

	server.scenario_cmd[0] = '\0';
	server.scenario_cmd_time = 0;

	if (!server.scenario_fd) return 0;

	char buf[256];
	if (!fgets(buf, sizeof(buf), server.scenario_fd)) {
		fclose(server.scenario_fd);
		server.scenario_fd = NULL;
		event_append(EVENT_SCENARIO_DONE);
		return 0;
	}

	double time = read_timestamp(buf);
	if (time < 0) return -1;

	char* cmd = strchr(buf, ' ');
	if (!cmd) return -1;
	while (*cmd == ' ') cmd++;

	char* p = strchr(cmd, '\n');
	if (p) *p = '\0';

	strcpy(server.scenario_cmd, cmd);
	server.scenario_cmd_time = time;

	return 0;
}


void server_process_events(void) {
	Event* e;
	double time = timestamp();
	while ((e = event_pop())) {

		event_print(e, time);

		Worker* w = e->worker;
		switch (e->type) {
		case EVENT_SCENARIO_START:
			if (server.scenario_fd) fclose(server.scenario_fd);
			server.scenario_fd = fopen(e->scenario, "r");
			if (server.scenario_fd) {
				server.scenario_timestamp = time;
				while (server_read_scenario_command()) {
					printf("error reading scenario command");
				}
			}
			else printf("could not open scenario %s\n", e->scenario);
			free(e->scenario);
			e->scenario = NULL;
			break;

		case EVENT_SCENARIO_DONE:
			break;

		case EVENT_WORKER_ONLINE:
			if (w->state == WORKER_BOOTING) {
				printf("worker %d booted successfully in %.2f seconds\n", w->id, time - w->timestamp);
			}
			w->state = WORKER_RUNNING;
			w->timestamp = time;
			racr_call_str("event-worker-online", "id", w->id, time);
			break;

		case EVENT_WORKER_OFFLINE:
			if (w->state != WORKER_HALTING) {
				printf("worker %d hung up unexpectedly\n", w->id);
				w->state = WORKER_ERROR;
				w->timestamp = time;
			}
			racr_call_str("event-worker-offline", "id", w->id, time);
			break;

		case EVENT_WORKER_OFF:
			w->state = WORKER_OFF;
			w->timestamp = time;
			w->socket_fd = -1;
			w->port = 0;
			cambri_set_mode(w->id, CAMBRI_OFF);
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
			w->state = WORKER_HALTING;
			w->timestamp = timestamp();
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


static void server_done(int sig) { server.running = 0; }


void server_run(int argc, char** argv) {

	if (argc == 2) {
		Event* e = event_append(EVENT_SCENARIO_START);
		e->scenario = strdup(argv[1]);
	}


	if (cambri_init()) printf("error initializing cambri\n");
	if (worker_init()) {
		printf("error initializing workers\n");
		return;
	}


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
	listen(listener, 5);

	FD_ZERO(&server.fds);
	FD_SET(STDIN, &server.fds);
	FD_SET(listener, &server.fds);
	server.fdmax = listener;
	server.work_counter = 0;
	server.timestamp = timestamp();
	server.running = 1;
	signal(SIGINT, server_done);

	server.log_fd = fopen("server.log", "w");

	printf("entering server loop\n");
	while (server.running) {

		server_process_events();

		// check for commands from scenario
		double time = timestamp();
		while (server.scenario_fd &&
		time >= server.scenario_timestamp + server.scenario_cmd_time) {
			server_command(server.scenario_cmd);
			while (server_read_scenario_command()) {
				printf("error reading scenario command");
			}
		}

		// receive
		fd_set fds = server.fds;
		struct timeval timeout = { 0, 50000 };
		int count = select(server.fdmax + 1, &fds, NULL, NULL, &timeout);
		if (count < 0) break;
		if (count > 0) {
			int i;
			for (i = 0; i <= server.fdmax; i++) {
				if (FD_ISSET(i, &fds)) {
					if (i == STDIN) {
						char cmd[256];
						if (fgets(cmd, sizeof(cmd), stdin)) {
							char* p = strchr(cmd, '\n');
							if (p) *p = '\0';
							server_command(cmd);
						}
					}
					else if (i == listener) server_new_connection(listener);
					else server_receive(i);
				}
			}
		}


		// turn off halting workers
		Worker* w;
		for (w = worker_next(NULL); w; w = worker_next(w)) {
			if (w->state == WORKER_HALTING
			&& time - w->timestamp > TIME_HALTING) {
				Event* e = event_append(EVENT_WORKER_OFF);
				e->worker = w;
			}
		}

		cambri_log_current(time - server.timestamp);
	}

	close(listener);

	cambri_kill();
	worker_kill();
	fclose(server.log_fd);
}
