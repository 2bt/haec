#include <stdio.h>
#include <error.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <arpa/inet.h>


int main(int argc, char** argv) {


	int sockfd = socket(AF_INET, SOCK_STREAM, 0);
	if (sockfd < 0) error(1, 0, "socket\n");
	struct sockaddr_in server = {
		AF_INET,
		htons(1337),
		{ inet_addr("127.0.0.1") }
	};


	if (connect(sockfd, (struct sockaddr*)&server, sizeof(server)) < 0) {
		close(sockfd);
		error(1, 0, "connect");
	}


	char s[INET_ADDRSTRLEN];
	inet_ntop(AF_INET, &server.sin_addr, s, sizeof(s));
	printf("client: connecting to %s\n", s);

	char msg[256];
	ssize_t l = recv(sockfd, msg, sizeof(msg), 0);
	fwrite(msg, 1, l, stdout);
	puts("");

	close(sockfd);

	return 0;
}
