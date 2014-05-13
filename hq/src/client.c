#include <stdio.h>
#include <error.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <arpa/inet.h>


int main(int argc, char** argv) {

	struct addrinfo hints = { 0, AF_INET, SOCK_STREAM };
	struct addrinfo* servinfo;
	if (getaddrinfo("localhost", "1337", &hints, &servinfo) != 0) {
		error(1, 0, "getaddrinfo");
	}
	int sockfd = socket(servinfo->ai_family,
						servinfo->ai_socktype,
						servinfo->ai_protocol);
	if (sockfd == -1) error(1, 0, "socket\n");


	if (connect(sockfd, servinfo->ai_addr, servinfo->ai_addrlen) == -1) {
		close(sockfd);
		perror("client: connect");
	}


	char s[INET_ADDRSTRLEN];
	struct sockaddr_in* addr_in = (struct sockaddr_in*) servinfo->ai_addr;
	inet_ntop(servinfo->ai_family, &addr_in->sin_addr, s, sizeof(s));
	printf("client: connecting to %s\n", s);
	freeaddrinfo(servinfo);

	close(sockfd);

	return 0;
}
