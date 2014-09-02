#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <error.h>
#include <termios.h>
#include <fcntl.h>
#include <unistd.h>

#include "cambri.h"



static int cambri_fd;

void cambri_init(void) {
	cambri_fd = open("/dev/ttyUSB0", O_RDWR | O_NOCTTY);
	if (cambri_fd < 0) error(1, 0, "cambri_init");

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


void cambri_write(const char* fmt, ...) {
	char buf[256];
	va_list args;
	va_start(args, fmt);
	vsprintf(buf, fmt, args);
	va_end(args);
	write(cambri_fd, buf, strlen(buf));
	write(cambri_fd, "\r\n", 2);
}


int cambri_read(char* buf, int len) {
	int i;
	int p = 0;
	for (i = 0; i < 200; i++) { // safety
		if (p >= 5 && strcmp(buf + p - 5, "\r\n>> ") == 0) return p;
		p += read(cambri_fd, buf + p, len - p);
	}
	return 0;
}

