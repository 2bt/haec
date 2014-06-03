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
	char line[1024];
	snprintf(line, sizeof(line),
		"../index/index mr %d ../index/wiki/dump_%04d.txt",
		args->threads, args->input_len);

	FILE* f = popen(line, "r");
	size_t len = fread(line, 1, sizeof(line), f);
	if (len <= 0) printf("work %d error\n", args->id);
	else printf("work %d output: %.*s", args->id, len, line);
	pclose(f);

	snprintf(line, sizeof(line), "work %d done", args->id);
	printf("%s\n", line);
	send(sockfd, line, strlen(line) + 1, 0);

	free(args);
	return NULL;
}


void handle_command(char* cmd) {
	char* p = strchr(cmd, ' ');
	if (!p) goto ERROR;
	*p++ = '\0';
	printf("command: %s %s\n", cmd, p);

	if (strcmp(cmd, "work") == 0) {
		WorkArgs* args = malloc(sizeof(WorkArgs));
		if (sscanf(p, "%d %d %d",
			&args->id, &args->threads, &args->input_len) != 3) goto ERROR;
		pthread_t work;
		pthread_create(&work, NULL, work_thread, args);
	}
	else if (strcmp(cmd, "cpu") == 0) {
		int cpus, freq;
		if (sscanf(p, "%d %d", &cpus, &freq) != 2) goto ERROR;

		// enable/disable 2nd cpu
		if (cpus == 1 || cpus == 2) {
			FILE* f = fopen("/sys/devices/system/cpu/cpu1/online", "w");
			if (!f) {
				printf("cpu error\n");
				return;
			}
			fprintf(f, cpus == 2 ? "1\n" : "0\n");
			fclose(f);
		}
		else goto ERROR;

		// change cpu freq
		printf("%d\n", system("cpufreq-set --governor userspace"));
		printf("%d\n", system("cpufreq-set --min 30000"));
		printf("%d\n", system("cpufreq-set --max 1008000"));
		char line[256];
		sprintf(line, "cpufreq-set --freq %d000", freq);
		printf("%d\n", system(line));

	}
	else goto ERROR;
	return;
ERROR:
	printf("error parsing command: %s\n", cmd);
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
