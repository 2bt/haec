#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <string.h>
#include <stdarg.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/un.h>
#include <unistd.h>
#include <error.h>
#include <errno.h>

#include "cambri.h"
#include "event.h"
#include "server.h"


#define BOOT_TIME	70.952
#define BOOT_POWER	1.2380610498

#define HALT_TIME	13.265
#define HALT_POWER	1.203496337

#define IDLE_POWER	1.29519747757
#define BUSY_POWER	2.44882204417


double SWITCH_POWER(int connections) {
	return 0.273665 + 0.213109 * connections;
}



double absolute_timestamp(void) {
	struct timespec t;
	clock_gettime(CLOCK_REALTIME, &t);
	return t.tv_sec + t.tv_nsec * 1e-9;
}


static ssize_t sendf(int s, const char* format, ...) {
	char line[256];
	va_list args;
	va_start(args, format);
	vsnprintf(line, sizeof(line), format, args);
	va_end(args);
	return send(s, line, strlen(line) + 1, 0);
}


// how much time is spent running scheme stuff
static double racr_total_time = 0;

static double racr_time;
void sim_racr_time_start(void) {
	racr_time = absolute_timestamp();
}
void sim_racr_time_end(void) {
	racr_total_time += absolute_timestamp() - racr_time;
}




static double sim_time = 0;
double sim_absolute_timestamp(void) { return sim_time; }


enum { OFF, BOOTING, CONNECTING, ONLINE, HALTING };
static const char* flag_strings[] = { "OFF", "BOOTING", "CONNECTING", "ONLINE", "HALTING" };


typedef struct {
	Worker* worker;
	int		socket_fd;
	int		work_id;
	double	busy_time;
	double	time;

	int		flag;

} WorkerState;


static WorkerState worker_states[NUM_CAMBRIS * 8] = {};


static void sim_debug(void) {
	printf("%s\n", format_timestamp(sim_time));
	Worker* w;
	for (w = worker_next(NULL); w; w = worker_next(w)) {
		int index = 8 * (w->id / 1000 - 1) + (w->id % 1000 - 1);
		WorkerState* state = &worker_states[index];
		if (w->is_switch) {
			printf("switch | %d | %-10s\n",
				w->id,
				state->flag ? "ON" : "OFF");
		}
		else {

			printf("worker | %d | %-10s | %3d | %6.1lf\n",
				w->id,
				flag_strings[state->flag],
				state->work_id,
				state->busy_time);
		}
	}
}


static void sim(double dt) {
	sim_time += dt;

	int i;
	for (i = 0; i < NUM_CAMBRIS * 8; i++) {

//		usleep(50);

		WorkerState* state = &worker_states[i];
		if (!state->worker) continue;
		if (state->worker->is_switch) continue;

		switch (state->flag) {
		case BOOTING:
			if (sim_time - state->time >= BOOT_TIME) state->flag = CONNECTING;
			break;


		case CONNECTING:
			state->socket_fd = socket(AF_UNIX, SOCK_STREAM, 0);
			if (state->socket_fd < 0) error(1, 0, "socket\n");

			int enable = 1;
			if (setsockopt(state->socket_fd, SOL_SOCKET, SO_REUSEADDR, &enable, sizeof(int)) < 0) {
				perror("SIM setsockopt");
				exit(1);
			}

			struct sockaddr_un client = { AF_UNIX };
			sprintf(client.sun_path, "%d", state->worker->id);
			unlink(client.sun_path);
			if (bind(state->socket_fd, (struct sockaddr*)&client, sizeof(client.sun_family) + strlen(client.sun_path)) < 0) {
				perror("SIM bind");
				exit(1);
			}

			struct sockaddr_un server = { AF_UNIX, "0000" };
			if (connect(state->socket_fd, (struct sockaddr*) &server, sizeof(server.sun_family) + strlen(server.sun_path)) < 0) {
				if (errno == ECONNREFUSED) printf("SIM connection refused\n");
				perror("SIM connect");
				close(state->socket_fd);
				break;
			}


			state->flag = ONLINE;
			break;


		case ONLINE:

			for (;;) {

				fd_set read_fds;
				FD_ZERO(&read_fds);
				FD_SET(state->socket_fd, &read_fds);
				struct timeval timeout = { 0, 0 };
				if (select(state->socket_fd + 1, &read_fds, NULL, NULL, &timeout) <= 0) break;

				char msg[1024];
				ssize_t len = recv(state->socket_fd, msg, sizeof(msg), 0);
				if (len <= 0) {
					printf("SIM server hung up!\n");
					close(state->socket_fd);
					state->flag = CONNECTING;
				}
				else {

					// parse commands
					msg[len] = '\0';
					char* cmd = msg;
					ssize_t pos = 0;
					while (pos < len) {
						ssize_t cmd_len = strlen(cmd);
						double load_size;
						if (sscanf(cmd, "work %d %lf", &state->work_id, &load_size) == 2) {
							state->time = sim_time;
							state->busy_time = 0.379926923077 + 0.262740722326 * load_size;
							sendf(state->socket_fd, "work-ack 0");
						}
						else if (strcmp(cmd, "halt") == 0) {
							state->flag = HALTING;
							state->time = sim_time;
							sendf(state->socket_fd, "halt-ack 0");
							close(state->socket_fd);
						}
						cmd += cmd_len + 1;
						pos += cmd_len + 1;
					}
				}
			}

			if (state->busy_time > 0) {
				// TODO: variable speed
				state->busy_time -= dt;
				if (state->busy_time <= 0) {
					state->busy_time = 0;
					sendf(state->socket_fd, "work-complete %d 0", state->work_id);
				}
			}

			break;

		case OFF:
		case HALTING:
		default:
			break;
		}

	}

//	puts("");
//	sim_debug();

};


extern FILE* cambri_log;
extern FILE* status_log;
extern int current_cache[NUM_CAMBRIS * 8];

void	sim_exit(void) {
	printf("racr-time: %lf\n", racr_total_time);
}

int		sim_cambri_init(void) {
	atexit(sim_exit);


	// init workers
	Worker* w;
	for (w = worker_next(NULL); w; w = worker_next(w)) {
		int index = 8 * (w->id / 1000 - 1) + (w->id % 1000 - 1);
		WorkerState* state = &worker_states[index];
		state->worker = w;

		if (w->is_switch) continue;

		// override worker address
		w->port = htons(w->id + 10000);
		w->addr.s_addr = inet_addr("127.0.0.1");
	}
	int i;
	cambri_log = fopen("cambri.log", "w");
	fprintf(cambri_log, " time      ");
	for (i = 0; i < NUM_CAMBRIS * 8; i++) fprintf(cambri_log, " | %5d", (i/8+1) * 1000 + i%8+1);
	fprintf(cambri_log, "\n");
	fprintf(cambri_log, "-----------");
	for (i = 0; i < NUM_CAMBRIS * 8; i++) fprintf(cambri_log, "-+------");
	fprintf(cambri_log, "\n");
	fflush(cambri_log);

	status_log = fopen("status.log", "w");

	return 0;
}


void	sim_cambri_kill(void) {
	fclose(cambri_log);
	fclose(status_log);
}


void	sim_cambri_write(int c, const char* fmt, ...) {
	sim_debug();
}
int		sim_cambri_read(int c, char* buf, int len) {

	return 0;
}


void sim_cambri_log_data(double time, char* scheduler) {

	int i;

	static double power_acc[NUM_CAMBRIS * 8] = {};
	static int sample_counter = 0;

	static int next_second = -1;
	if (next_second < 0) next_second = (int) time + 1;

	if (time > next_second) {

		fprintf(status_log, "%s", format_timestamp(next_second));
		fprintf(status_log, " energy:%.1f", cambri_get_energy());
		fprintf(status_log, " scheduler:%s", scheduler);
		fprintf(status_log, "\n");
		fflush(status_log);


		while (time > next_second) {
			fprintf(cambri_log, "%s", format_timestamp(next_second));
			next_second++;
			for (i = 0; i < NUM_CAMBRIS * 8; i++) {
				double power = power_acc[i] / sample_counter;
				cambri_set_energy(cambri_get_energy() + power);
				fprintf(cambri_log, " | %4.3f", power);
			}
			fprintf(cambri_log, "\n");
		}

		fflush(cambri_log);


		for (i = 0; i < NUM_CAMBRIS * 8; i++) power_acc[i] = 0;
		sample_counter = 0;
	}
	sample_counter++;


	for (i = 0; i < NUM_CAMBRIS * 8; i++) {

		double power = 0;

		WorkerState* state = &worker_states[i];
		if (state->worker) {
			if (state->worker->is_switch) {

				if (state->flag != OFF) {
					int connections = 1;
					int j;
					for (j = 0; j < NUM_CAMBRIS * 8; j++) {
						WorkerState* s = &worker_states[j];
						if (s->flag != OFF && s->worker->parent_id == state->worker->id) connections++;
					}
					power = SWITCH_POWER(connections);
				}
			}
			else {
				switch (state->flag) {
				case BOOTING:		power = BOOT_POWER; break;
				case CONNECTING:	power = IDLE_POWER; break;
				case ONLINE:		power = (state->busy_time == 0) ? IDLE_POWER : BUSY_POWER; break;
				case HALTING:		power = HALT_POWER; break;
				default: break;
				}
			}
		}

		power_acc[i] += power;
		current_cache[i] = power / 0.001 / 5.1;
	}
}


void	sim_cambri_set_mode(int id, int mode) {
	int i = 8 * (id / 1000 - 1) + (id % 1000 - 1);
	WorkerState* state = &worker_states[i];

	if (mode == CAMBRI_CHARGE) {
		if (state->flag == OFF) {
			state->flag = BOOTING;
			state->time = sim_time;
			state->time = timestamp();
		}
	}
	else {
		state->flag = OFF;
	}
}


int sim_select(int nfds, fd_set* readfds, fd_set* writefds, fd_set* exceptfds, struct timeval* timeout) {
	double t = timeout->tv_sec + timeout->tv_usec * 1e-6;

	for (;;) {
		struct timeval to = { 0, 0 };
		int r = select(nfds, readfds, writefds, exceptfds, &to);
		if (r != 0 || t <= 0) return r;

		sim(0.05);
		t -= 0.05;
	}
}
