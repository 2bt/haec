#include <stdio.h>
#include <error.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <arpa/inet.h>

#include <racr/racr.h>


enum { STDIN = 0 };


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


int main(int argc, char** argv) {

	RACR_INIT(env, "bytecode", NULL);
	eval_script(env, "scheme.scm");


	// tcp server code
	int listener = socket(AF_INET, SOCK_STREAM, 0);
	if (listener == -1) error(1, 0, "socket\n");
	int yes = 1;
	if (setsockopt(listener, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int)) < 0) {
		error(1, 0, "setsockopt");
	}
	struct sockaddr_in server = {
		AF_INET,
		htons(1337),
		{ INADDR_ANY }
	};
	if (bind(listener, (struct sockaddr*)&server, sizeof(server)) < 0) {
		error(1, 0, "bind");
	}
	listen(listener, 3);

	fd_set master;
	FD_ZERO(&master);
	FD_SET(STDIN, &master);
	FD_SET(listener, &master);
	int fdmax = listener;



	for (;;) {
		fd_set readfds = master;
		if (select(fdmax + 1, &readfds, NULL, NULL, NULL) < 0) {
			error(1, 0, "select");
		}

		int i;
		for (i = 0; i <= fdmax; i++) {
			if (FD_ISSET(i, &readfds)) {
				if (i == STDIN) {
					char msg[256];
					fgets(msg, sizeof(msg), stdin);
					printf("stdin: %s", msg);
				}
				else if (i == listener) {
					// handle new connection

					struct sockaddr_in client;
					socklen_t size = sizeof(client);
					int newfd = accept(listener, (struct sockaddr *)&client, &size);
					if (newfd < 0) {
						perror("accept");
					}
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
					int len = recv(i, msg, sizeof(msg), 0);
					if (len <= 0) {
						close(i);
						FD_CLR(i, &master);
						printf("socket %d hung up\n", i);
					}
					else {
						printf("received %d bytes from socket %d: %s\n", len, i, msg);


					}
				}
			}
		}

	}


	return 0;
}
