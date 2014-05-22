#include <stdio.h>
#include <error.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <arpa/inet.h>

#include <racr/racr.h>



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
	int sockfd = socket(AF_INET, SOCK_STREAM, 0);
	if (sockfd == -1) error(1, 0, "socket\n");
	int yes = 1;
	if (setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int)) < 0) {
		error(1, 0, "setsockopt");
	}
	struct sockaddr_in server = {
		AF_INET,
		htons(1337),
		{ INADDR_ANY }
	};
	if (bind(sockfd, (struct sockaddr*)&server, sizeof(server)) < 0) {
		error(1, 0, "bind");
	}
	listen(sockfd, 3);


	for (;;) {
		struct sockaddr_in client;
		socklen_t size = sizeof(client);
		int new_fd = accept(sockfd, (struct sockaddr *)&client, &size);
		if (new_fd < 0) {
			perror("accept");
			continue;
		}

		char s[INET_ADDRSTRLEN];
		inet_ntop(client.sin_family, &client.sin_addr, s, sizeof(s));
		printf("server: got connection from %s\n", s);


		if (send(new_fd, "Hello, world!", 13, 0) < 0) {
			perror("send");
		}
		close(new_fd);
	}

	return 0;
}
