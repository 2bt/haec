#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <error.h>
#include <termios.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <arpa/inet.h>

#include <racr/racr.h>


enum { STDIN = 0 };

enum { PORT = 1337 };


typedef struct {
	int cambri_port;
	int socket_fd;


} Worker;

enum { WORKER_COUNT = 14 };

Worker workers[WORKER_COUNT];


Scheme_Object* eval_script(Scheme_Env* env, const char* filename) {
	FILE* f = fopen(filename, "r");
	if (!f) error(1, 0, "Error opening file: %s", filename);
	fseek(f, 0, SEEK_END);
	long size = ftell(f);
	rewind(f);
	char source[size + 1];
	fread(source, 1, size, f);
	source[size] = '\0';
	return scheme_eval_string_all(source, env, 1);
}



int cambri_fd;

void cambri_init(void) {
	cambri_fd = open("/dev/ttyUSB0", O_RDWR | O_NOCTTY);
	if (cambri_fd < 0) error(1, 0, "open");

	struct termios tty = {};
	tcgetattr(cambri_fd, &tty);
	tty.c_lflag = 0;
	tty.c_oflag = 0;
	tty.c_iflag = 0;
	tty.c_cflag = CS8 | CLOCAL | CREAD;
	tty.c_cc[VTIME] = 1; // 0.1 s read timeout
	cfsetospeed(&tty, B115200);
	cfsetispeed(&tty, B115200);
	tcsetattr(cambri_fd, TCSANOW, &tty);
}

void cambri_kill(void) {
	close(cambri_fd);
}

void cambri_write(char* cmd) {
	write(cambri_fd, cmd, strlen(cmd));
	write(cambri_fd, "\r\n", 2);
}
void cambri_read(char* buf) {
	int i;
	for (i = 0; i < 1000; i++) { // safety
		int p = strlen(buf);
		if (p >= 5 && strcmp(buf + p - 5, "\r\n>> ") == 0) break;
		read(cambri_fd, buf + p, sizeof(buf) - p);
	}
}


void serve(void) {

	int listener = socket(AF_INET, SOCK_STREAM, 0);
	if (listener < 0) error(1, 0, "socket\n");
	int yes = 1;
	if (setsockopt(listener, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int)) < 0) {
		error(1, 0, "setsockopt");
	}
	struct sockaddr_in server = { AF_INET, htons(PORT), { INADDR_ANY } };
	if (bind(listener, (struct sockaddr*)&server, sizeof(server)) < 0) {
		error(1, 0, "bind");
	}
	listen(listener, 3);

	fd_set master;
	FD_ZERO(&master);
	FD_SET(STDIN, &master);
	FD_SET(listener, &master);
	int fdmax = listener;


	printf("entering event loop.\n");
	int running = 1;
	while (running) {
		fd_set readfds = master;
		if (select(fdmax + 1, &readfds, NULL, NULL, NULL) < 0) {
			error(1, 0, "select");
		}

		int i;
		for (i = 0; i <= fdmax; i++) {
			if (FD_ISSET(i, &readfds)) {


				if (i == STDIN) {
					// command from stdin

					char msg[1024];
					fgets(msg, sizeof(msg), stdin);
					msg[strlen(msg) - 1] = '\0';
					printf("stdin: %s\n", msg);


					if (strcmp(msg, "exit")) {
						running = 0;
						printf("exiting...\n");
					}
					else {
						// TESTING
						send(fdmax, msg, strlen(msg) + 1, 0);
					}

				}

				else if (i == listener) {
					// new connection

					struct sockaddr_in client;
					socklen_t size = sizeof(client);
					int newfd = accept(listener, (struct sockaddr*)&client, &size);
					if (newfd < 0) perror("accept");
					else {
						FD_SET(newfd, &master);
						if (newfd > fdmax) fdmax = newfd;

						char s[INET_ADDRSTRLEN];
						inet_ntop(client.sin_family, &client.sin_addr, s, sizeof(s));
						printf("new connection from %s on socket %d\n", s, newfd);
					}
				}

				else {
					// recv from client
					char msg[1024];
					ssize_t len = recv(i, msg, sizeof(msg), 0);
					if (len <= 0) {
						close(i);
						FD_CLR(i, &master);
						printf("socket %d hung up\n", i);
					}
					else {
						printf("received %d bytes from socket %d: %.*s\n",
							len, i, len, msg);
					}
				}
			}
		}
	}

	close(listen);
}


int main(int argc, char** argv) {

	RACR_INIT(env, "bytecode", NULL);
	eval_script(env, "scheme.scm");

	cambri_init();

	serve();

	cambri_kill();

	return 0;
}
