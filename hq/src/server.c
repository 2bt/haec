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

	struct addrinfo hints = { AI_PASSIVE, AF_INET, SOCK_STREAM };
	struct addrinfo* servinfo;
	if (getaddrinfo(NULL, "1337", &hints, &servinfo) != 0) {
		error(1, 0, "getaddrinfo");
	}

	int sockfd = socket(servinfo->ai_family,
						servinfo->ai_socktype,
						servinfo->ai_protocol);
	if (sockfd == -1) error(1, 0, "socket\n");

	int yes = 1;
	if (setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int)) == -1) {
		error(1, 0, "setsockopt");
	}
	if (bind(sockfd, servinfo->ai_addr, servinfo->ai_addrlen) == -1) {
		error(1, 0, "bind");
	}

	freeaddrinfo(servinfo);

	listen(sockfd, 10);

	for (;;) {
		struct sockaddr_in their_addr;
		socklen_t sin_size = sizeof(their_addr);
		int new_fd = accept(sockfd, (struct sockaddr *)&their_addr, &sin_size);
		if (new_fd == -1) {
			perror("accept");
			continue;
		}


		char s[INET_ADDRSTRLEN];
		inet_ntop(their_addr.sin_family, &their_addr.sin_addr, s, sizeof(s));
		printf("server: got connection from %s\n", s);



		if (send(new_fd, "Hello, world!", 13, 0) == -1) {
			perror("send");
		}
		close(new_fd);
	}


	return 0;
}
