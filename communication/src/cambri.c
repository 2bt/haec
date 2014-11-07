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



static int cambri_fds[2];
static FILE* cambri_log_file;




int cambri_init(void) {
	int ret = 0;

	int c, i;
	for (c = 0; c < 2; c++) {
		int fd = open(!c ? "/dev/ttyUSB0" : "/dev/ttyUSB1", O_RDWR | O_NOCTTY);
		if (fd < 0) {
			cambri_fds[c] = 0;
			ret++;
		}
		else {
			cambri_fds[c] = fd;

			struct termios tty = {};
			tcgetattr(fd, &tty);
			tty.c_lflag = 0;
			tty.c_oflag = 0;
			tty.c_iflag = 0;
			tty.c_cflag = CS8 | CLOCAL | CREAD;
			tty.c_cc[VTIME] = 1; // 0.1 s read timeout
			tty.c_cc[VMIN] = 0; // non-blocking
			cfsetospeed(&tty, B115200);
			cfsetispeed(&tty, B115200);
			tcsetattr(fd, TCSANOW, &tty);

			// enable profile 4 only
			char buf[1024] = {};
			for (i = 1; i <= 5; i++) {
				cambri_write(c, "en_profile %d %d", i, i == 4);
				if (cambri_read(c, buf, sizeof(buf)) == 0) {
					error(1, 0, "cambri_init");
				}
			}
		}
	}


	cambri_log_file = fopen("cambri.log", "w");
	fprintf(cambri_log_file, " time      ");
	for (i = 0; i < 16; i++) fprintf(cambri_log_file, " | %4d", (i/8+1) * 1000 + i%8+1);
	fprintf(cambri_log_file, "\n");
	fprintf(cambri_log_file, "-----------");
	for (i = 0; i < 16; i++) fprintf(cambri_log_file, "-+-----");
	fprintf(cambri_log_file, "\n");
	fflush(cambri_log_file);

	return ret;
}


void cambri_kill(void) {
	int c;
	for (c = 0; c < 2; c++) {
		if (cambri_fds[c]) close(cambri_fds[c]);
	}
	fclose(cambri_log_file);
}


void cambri_write(int c, const char* fmt, ...) {
	if (c < 0 || c >= 2 || !cambri_fds[c]) return;

	char buf[1024];
	va_list args;
	va_start(args, fmt);
	int len = vsprintf(buf, fmt, args);
	va_end(args);
	write(cambri_fds[c], buf, len);
	write(cambri_fds[c], "\r\n", 2);
}


int cambri_read(int c, char* buf, int len) {
	if (c < 0 || c >= 2 || !cambri_fds[c]) return 0;

	int p = 0;
	int i = 0;
	while (i < 5) { // safety
		if (p >= 5 && strcmp(buf + p - 5, "\r\n>> ") == 0) break;
		int d = read(cambri_fds[c], buf + p, len - p);
		i += d == 0;
		p += d;
	}

	if (p >= 10 && strcmp(buf + p - 10, "\r\nboot>> \n") == 0) {
		printf("CAMBRI ERROR\n");
//		cambri_write(c, "reboot");
	}
	return p;
}


void cambri_log_current(double time) {
	int i;

	static int current_acc[16] = {};
	static int sample_counter = 0;

	static int next_second = -1;
	if (next_second < 0) next_second = (int) time + 1;

	if (time > next_second) {

		while (time > next_second) {
			fprintf(cambri_log_file, "%s", format_timestamp(next_second));
			next_second++;
			for (i = 0; i < 16; i++) {
				fprintf(cambri_log_file, " | %4d", current_acc[i] / sample_counter);
			}
			fprintf(cambri_log_file, "\n");
		}


		fflush(cambri_log_file);


		for (i = 0; i < 16; i++) current_acc[i] = 0;
		sample_counter = 0;
	}
	sample_counter++;


	int c;
	for (c = 0; c < 2; c++) {
		if (!cambri_fds[c]) continue;

		char buf[1024] = {};
		cambri_write(c, "state");
		int ret = cambri_read(c, buf, sizeof(buf));
		if (ret == 0) error(1, 0, "cambri_log_current");
		char* p = buf;

		for (i = 0; i < 8; i++) {
			p = strchr(p, '\n');
			if (!p) error(1, 0, "cambri_log_current2");
			int current = atoi(p += 5);
			current_acc[c*8 + i] += current;
		}
	}
}


void cambri_set_mode(int id, int mode) {
	int c = id / 1000 - 1;
	if (c < 0 || c >= 2 || !cambri_fds[c]) return;
	cambri_write(c, "mode %c %d 4", mode, id % 10);
	char buf[1024] = {};
	cambri_read(c, buf, sizeof(buf));
}

