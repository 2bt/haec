#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <error.h>
#include <termios.h>
#include <fcntl.h>
#include <unistd.h>

#include "cambri.h"
#include "event.h"



static int cambri_fd;
static FILE* cambri_log_file;




int cambri_init(void) {
	cambri_fd = open("/dev/ttyUSB0", O_RDWR | O_NOCTTY);
	if (cambri_fd < 0) cambri_fd = open("/dev/ttyUSB1", O_RDWR | O_NOCTTY);

	if (cambri_fd < 0) return -1;

	struct termios tty = {};
	tcgetattr(cambri_fd, &tty);
	tty.c_lflag = 0;
	tty.c_oflag = 0;
	tty.c_iflag = 0;
	tty.c_cflag = CS8 | CLOCAL | CREAD;
	tty.c_cc[VTIME] = 1; // 0.1 s read timeout
	tty.c_cc[VMIN] = 0; // non-blocking
	cfsetospeed(&tty, B115200);
	cfsetispeed(&tty, B115200);
	tcsetattr(cambri_fd, TCSANOW, &tty);


	int i;
	// enable profile 4 only
	char buf[1024] = {};
	for (i = 1; i <= 5; i++) {
		cambri_write("en_profile %d %d", i, i == 4);
		cambri_read(buf, sizeof(buf));
	}

	cambri_log_file = fopen("cambri.log", "w");
	fprintf(cambri_log_file, " time      ");
	for (i = 1; i <= 8; i++) fprintf(cambri_log_file, " | %4d", 1000 + i);
	fprintf(cambri_log_file, "\n");
	fprintf(cambri_log_file, "-----------");
	for (i = 1; i <= 8; i++) fprintf(cambri_log_file, "-+-----");
	fprintf(cambri_log_file, "\n");
	fflush(cambri_log_file);

	return 0;
}


void cambri_kill(void) {
	if (cambri_fd < 0) return;
	close(cambri_fd);
	fclose(cambri_log_file);
}


void cambri_write(const char* fmt, ...) {
	char buf[1024];
	va_list args;
	va_start(args, fmt);
	int len = vsprintf(buf, fmt, args);
	va_end(args);
	write(cambri_fd, buf, len);
	write(cambri_fd, "\r\n", 2);
}


int cambri_read(char* buf, int len) {
	int p = 0;
	int i = 0;
	while (i < 5) { // safety
		if (p >= 5 && strcmp(buf + p - 5, "\r\n>> ") == 0) break;
		int d = read(cambri_fd, buf + p, len - p);
		i += d == 0;
		p += d;
	}

	if (p >= 10 && strcmp(buf + p - 10, "\r\nboot>> \n") == 0) {
//		printf("CAMBRI ERROR\n");
//		cambri_write("reboot");
	}
	return p;
}


void cambri_log_current(const char* time) {
	if (cambri_fd < 0) return;
	cambri_write("state");
	char buf[1024] = {};
	int ret = cambri_read(buf, sizeof(buf));
	if (ret == 0) error(1, 0, "cambri_log_current");

	fprintf(cambri_log_file, "%s", time);

	char* p = buf;
	int i;
	for (i = 0; i < 8; i++) {
		p = strchr(p, '\n') + 5;
		int current = atoi(p);
		fprintf(cambri_log_file, " | %4d", current);
	}
	fprintf(cambri_log_file, "\n");
	fflush(cambri_log_file);
}


void cambri_set_mode(int id, int mode) {
	if (cambri_fd < 0) return;
	cambri_write("mode %c %d 4", mode, id % 10);
	char buf[1024] = {};
	cambri_read(buf, sizeof(buf));
}

