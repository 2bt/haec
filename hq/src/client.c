#include <stdio.h>
#include <error.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <arpa/inet.h>


int main(int argc, char** argv) {

	if (argc != 2) {
		printf("usage: %s server-address\n", argv[0]);
		return 0;
	}


	int sockfd = socket(AF_INET, SOCK_STREAM, 0);
	if (sockfd < 0) error(1, 0, "socket\n");
	struct sockaddr_in server = {
		AF_INET,
		htons(1337),
		{ inet_addr(argv[1]) }
	};
	if (connect(sockfd, (struct sockaddr*)&server, sizeof(server)) < 0) {
		close(sockfd);
		error(1, 0, "connect");
	}
	char s[INET_ADDRSTRLEN];
	inet_ntop(AF_INET, &server.sin_addr, s, sizeof(s));
	printf("client: connecting to %s\n", s);



	send(sockfd, "hallo", 5, 0);

	char msg[256];
	ssize_t l = recv(sockfd, msg, sizeof(msg), 0);
	fwrite(msg, 1, l, stdout);
	puts("");


	close(sockfd);

	return 0;
}
