#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <error.h>
#include <errno.h>

#include <unistd.h>
#include <pthread.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <arpa/inet.h>


enum { PORT = 1337 };


const char* led_path = NULL;
void set_led(int flag) {
	if (!led_path) return;
	FILE* f = fopen(led_path, "w");
	fprintf(f, flag ? "255" : "0");
	fclose(f);
}


ssize_t sendf(int s, const char* format, ...) {
	char line[256];
	va_list args;
	va_start(args, format);
	vsnprintf(line, sizeof(line), format, args);
	va_end(args);
	printf("send: %s\n", line);
	return send(s, line, strlen(line) + 1, 0);
}


int socket_fd;
int thread_count;


typedef struct { int id; double load_size; } WorkArgs;


void* work_thread(WorkArgs* args) {

	set_led(1);

	char line[256];
	snprintf(line, sizeof(line),
		"../index/index mr %d ../index/wiki/dump.txt %d",
		thread_count, (unsigned) (args->load_size * 1024 * 1024)); // convert MB to Bytes

	FILE* f = popen(line, "r");
	size_t len = fread(line, 1, sizeof(line), f);
	if (len <= 0) printf("work-error %d\n", args->id);
	else printf("work %d output: %.*s", args->id, (int) len, line);
	int ret = pclose(f);

	sendf(socket_fd, "work-complete %d %d", args->id, ret);

	free(args);

	set_led(0);

	return NULL;
}


int running = 1;


void handle_command(char* cmd) {
	printf("command: %s\n", cmd);

	char* p = strchr(cmd, ' ');
	if (p) *p++ = '\0';

	if (strcmp(cmd, "work") == 0) {

		// spawn work thread
		if (!p) goto ERROR;
		WorkArgs* args = malloc(sizeof(WorkArgs));
		if (sscanf(p, "%d %lf",
			&args->id, &args->load_size) != 2) goto ERROR;
		pthread_t work;
		int e = pthread_create(&work, NULL, (void*(*)(void*)) work_thread, args);
		sendf(socket_fd, "work-ack %d", e);
	}
	else if (strcmp(cmd, "cpu") == 0) {

		if (!p) goto ERROR;
		int cpus, freq;
		if (sscanf(p, "%d %d", &cpus, &freq) != 2) goto ERROR;

		// enable/disable 2nd cpu
		if (cpus != 1 && cpus != 2) goto ERROR;
		FILE* f = fopen("/sys/devices/system/cpu/cpu1/online", "w");
		if (!f) {
			sendf(socket_fd, "cpu-ack 1");
			return;
		}
		fprintf(f, cpus == 2 ? "1\n" : "0\n");
		fclose(f);

		// change cpu freq
		char line[1024];
		sprintf(line,
			"cpufreq-set --governor userspace && "
			"cpufreq-set --min 30000 && "
			"cpufreq-set --max 1008000 && "
			"cpufreq-set --freq %d000", freq);
		if (system(line) != 0) {
			sendf(socket_fd, "cpu-ack 2");
			return;
		}
		sendf(socket_fd, "cpu-ack 0");

	}
	else if (strcmp(cmd, "mem") == 0) {
		FILE* f = popen("/usr/bin/free", "r");
		if (!f) {
			sendf(socket_fd, "mem-ack -1");
			return;
		}
		int mem = -1;
		fscanf(f, "%*s %*s %*s %*s %*s %*s %*s %*d %*d %d", &mem);
		fclose(f);
		sendf(socket_fd, "mem-ack %d", mem);
	}
	else if (strcmp(cmd, "halt") == 0) {
		int e = system("halt");
		sendf(socket_fd, "halt-ack %d", e);
		if (e == 0) running = 0;
	}

	else goto ERROR;
	return;
ERROR:
	sendf(socket_fd, "%s-ack 1337", cmd);
}



int main(int argc, char** argv) {

	if (argc > 2) {
		printf("usage: %s server-address\n", argv[0]);
		return 0;
	}

	// get thread count
	{
		FILE* f = popen("nproc", "r");
		char line[64];
		size_t len = fread(line, 1, sizeof(line), f);
		pclose(f);
		if (len > 0) thread_count = atoi(line) - 1;
		else thread_count = 0;
	}

	// init led path
	{
		const char* led_paths[2] = {
			"/sys/class/leds/blue:ph21:led2/brightness",
			"/sys/class/leds/d2"
		};
		int i;
		for (i = 0; i < 2; i++) {
			if (access(led_paths[i], W_OK) != -1) {
				led_path = led_paths[i];
				break;
			}
		}
	}


	const char* addr = argc == 2 ? argv[1] : "127.0.0.1";
	struct sockaddr_in server = { AF_INET, htons(PORT), { inet_addr(addr) } };

	int connected = 0;
	socket_fd = socket(AF_INET, SOCK_STREAM, 0);
	if (socket_fd < 0) error(1, 0, "socket\n");

	while (running) {

		while (!connected) {
			printf("trying to connect to server at %s\n", addr);
			if (connect(socket_fd, (struct sockaddr*) &server, sizeof(server)) < 0) {
				if (errno == ECONNREFUSED) {
					printf("connection refused\n");
					sleep(1);
				}
				else {
					close(socket_fd);
					perror("main");
					error(1, 0, "connect");
				}
			}
			else connected = 1;
		}

		char msg[1024];
		ssize_t len = recv(socket_fd, msg, sizeof(msg), 0);
		if (len <= 0) {
			printf("server hung up\n");
			close(socket_fd);
			connected = 0;
			sleep(1);

			socket_fd = socket(AF_INET, SOCK_STREAM, 0);
			if (socket_fd < 0) error(1, 0, "socket\n");

			continue;
		}

		// parse commands
		msg[len] = '\0';
		char* cmd = msg;
		ssize_t pos = 0;
		while (pos < len) {
			ssize_t cmd_len = strlen(cmd);
			handle_command(cmd);
			fflush(stdout);
			cmd += cmd_len + 1;
			pos += cmd_len + 1;
		}
	}
	close(socket_fd);

	return 0;
}
