#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <error.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <arpa/inet.h>


enum { PORT = 1337 };


int sockfd;


typedef struct {
	int id;
	int threads;
	int input_len;
} WorkArgs;


void* work_thread(WorkArgs* args) {
	const char* index_path = "/home/cubie/haec/communication";
	char line[256];
	snprintf(line, sizeof(line),
		"%s/index mr %d %s/wiki/dump_%04d.txt",
		index_path, args->threads, index_path, args->input_len);

	FILE* f = popen(line, "r");
	size_t len = fread(line, 1, sizeof(line), f);
	if (len <= 0) printf("work %d error\n", args->id);
	else printf("work %d output: %.*s", args->id, len, line);
	int ret = pclose(f);

	snprintf(line, sizeof(line), "work %d done %d", args->id, ret);
	printf("%s\n", line);
	send(sockfd, line, strlen(line) + 1, 0);

	free(args);
	return NULL;
}



void send_ack(int e) {
	char line[64];
	snprintf(line, sizeof(line), "ack %d", e);
	send(sockfd, line, strlen(line) + 1, 0);
}


void handle_command(char* cmd) {
	printf("command: %s\n", cmd);

	char* p = strchr(cmd, ' ');
	if (p) *p++ = '\0';

	if (strcmp(cmd, "work") == 0) {

		// spawn work thread
		if (!p) goto ERROR;
		WorkArgs* args = malloc(sizeof(WorkArgs));
		if (sscanf(p, "%d %d %d",
			&args->id, &args->threads, &args->input_len) != 3) goto ERROR;
		pthread_t work;
		int e = pthread_create(&work, NULL, work_thread, args);
		if (e != 0) printf("work error %d\n", e);
		send_ack(e);
	}
	else if (strcmp(cmd, "cpu") == 0) {

		if (!p) goto ERROR;
		int cpus, freq;
		if (sscanf(p, "%d %d", &cpus, &freq) != 2) goto ERROR;

		// enable/disable 2nd cpu
		if (cpus != 1 && cpus != 2) goto ERROR;
		FILE* f = fopen("/sys/devices/system/cpu/cpu1/online", "w");
		if (!f) {
			printf("cpu error 1\n");
			send_ack(1);
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
			printf("cpu error 2\n");
			send_ack(2);
			return;
		}
		send_ack(0);

	}
	else if (strcmp(cmd, "halt") == 0) {

		int e = system("halt");
		if (e != 0) printf("halt error %d\n", e);
		send_ack(e);
	}

	else goto ERROR;
	return;
ERROR:
	printf("error parsing command: %s\n", cmd);
	send_ack(1337);
}



int main(int argc, char** argv) {

	if (argc > 2) {
		printf("usage: %s server-address\n", argv[0]);
		return 0;
	}

	sockfd = socket(AF_INET, SOCK_STREAM, 0);
	if (sockfd < 0) error(1, 0, "socket\n");
	struct sockaddr_in server = { AF_INET, htons(PORT),
		{ inet_addr(argc == 2 ? argv[1] : "127.0.0.1" ) }
	};
	if (connect(sockfd, (struct sockaddr*)&server, sizeof(server)) < 0) {
		close(sockfd);
		error(1, 0, "connect");
	}

	for (;;) {
		char msg[1024];
		ssize_t len = recv(sockfd, msg, sizeof(msg), 0);
		if (len <= 0) {
			printf("server hung up\n");
			close(sockfd);
			break;
		}
		// parse commands
		msg[len] = '\0';
		char* cmd = msg;
		ssize_t pos = 0;
		while (pos < len) {
			ssize_t cmd_len = strlen(cmd);
			handle_command(cmd);
			cmd += cmd_len + 1;
			pos += cmd_len + 1;
		}

	}

	return 0;
}
