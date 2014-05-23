#include <stdio.h>
#include <string.h>
#include <error.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <arpa/inet.h>


enum { PORT = 1337 };


int sockfd;


void* worker_thread(void* data) {
	printf("worker hi\n");

	char line[1024];
	snprintf(line, sizeof(line), "../index/index -mr 0 ../index/index/wiki/test_25.txt");
	FILE* p = popen(line, "r");
	size_t len = fread(line, 1, sizeof(line), p);
	if (len <= 0) {
		printf("worker error\n");
	}
	else {
		printf("index output: %s", line);
	}

	pclose(p);

	printf("worker bye\n");
	send(sockfd, "worker bye", 10, 0);

	return NULL;
}


void handle_command(char* cmd) {
	char* p = strchr(cmd, ' ');
	if (p) *p++ = '\0';
	if (strcmp(cmd, "index") == 0) {
		printf("command: index [%s]\n", p);
		pthread_t worker;
		pthread_create(&worker, NULL, worker_thread, NULL);



	}
	else {
		printf("unknown command: %s [%s]\n", cmd, p);
	}
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
