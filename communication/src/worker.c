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


void* worker_thread(char* arg) {
	if (arg) { // parse arg string
		char* p = strchr(arg, ' ');
		if (p) *p++ = '\0';
	}

	char line[1024];
	snprintf(line, sizeof(line), "../index/index mr 0 ../index/wiki/test_25.txt");
	FILE* f = popen(line, "r");
	size_t len = fread(line, 1, sizeof(line), f);
	if (len <= 0) printf("worker %s error\n", arg);
	else printf("worker %s output: %.*s", arg, len, line);
	pclose(f);

	snprintf(line, sizeof(line), "worker %s done", arg);
	printf("%s\n", line);
	send(sockfd, line, strlen(line) + 1, 0);

	free(arg);
	return NULL;
}


void handle_command(char* cmd) {
	char* p = strchr(cmd, ' ');
	if (!p) {
		printf("error parsing command: %s\n", cmd);
		return;
	}
	*p++ = '\0';
	printf("command: %s %s\n", cmd, p);

	if (strcmp(cmd, "worker") == 0) {
		pthread_t worker;
		pthread_create(&worker, NULL, worker_thread, strdup(p));
	}
	if (strcmp(cmd, "cpu") == 0) {


	}
	else printf("unknown command\n");
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
