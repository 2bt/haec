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


enum { NUM_CAMBRIS = 1 };


static int cambri_fds[NUM_CAMBRIS];
static FILE* cambri_log_file;


static const char* get_tty_name(int c) {
	if (c == 0) {
		if (access("/sys/devices/platform/sw-ohci.1/usb2/2-1/2-1:1.0/ttyUSB0", F_OK) != -1) return "/dev/ttyUSB0";
		if (access("/sys/devices/platform/sw-ohci.1/usb2/2-1/2-1:1.0/ttyUSB1", F_OK) != -1) return "/dev/ttyUSB1";
	}
	if (c == 1) {
		if (access("/sys/devices/platform/sw-ohci.2/usb4/4-1/4-1:1.0/ttyUSB0", F_OK) != -1) return "/dev/ttyUSB0";
		if (access("/sys/devices/platform/sw-ohci.2/usb4/4-1/4-1:1.0/ttyUSB1", F_OK) != -1) return "/dev/ttyUSB1";
	}
	return NULL;
}


int cambri_init(void) {
	int ret = 0;

	int c, i;
	for (c = 0; c < NUM_CAMBRIS; c++) {
		const char* name = get_tty_name(c);
		int fd = -1;
		if (name) fd = open(name, O_RDWR | O_NOCTTY);

		if (!name || fd < 0) {
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
	for (i = 0; i < NUM_CAMBRIS * 8; i++) fprintf(cambri_log_file, " | %4d", (i/8+1) * 1000 + i%8+1);
	fprintf(cambri_log_file, "\n");
	fprintf(cambri_log_file, "-----------");
	for (i = 0; i < NUM_CAMBRIS * 8; i++) fprintf(cambri_log_file, "-+-----");
	fprintf(cambri_log_file, "\n");
	fflush(cambri_log_file);

	return ret;
}


void cambri_kill(void) {
	int c;
	for (c = 0; c < NUM_CAMBRIS; c++) {
		if (cambri_fds[c]) close(cambri_fds[c]);
	}
	fclose(cambri_log_file);
}


void cambri_write(int c, const char* fmt, ...) {
	if (c < 0 || c >= NUM_CAMBRIS || !cambri_fds[c]) return;

	char buf[1024];
	va_list args;
	va_start(args, fmt);
	int len = vsprintf(buf, fmt, args);
	va_end(args);
	write(cambri_fds[c], buf, len);
	write(cambri_fds[c], "\r\n", 2);
}


int cambri_read(int c, char* buf, int len) {
	if (c < 0 || c >= NUM_CAMBRIS || !cambri_fds[c]) return 0;

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


static double energy = 0;
static int current_cache[NUM_CAMBRIS * 8] = {};

int		cambri_get_current(int id) { return current_cache[id]; }
double	cambri_get_energy(void) { return energy; }
void	cambri_set_energy(double e) { energy = e; }


void cambri_log_power(double time) {
	int i;

	static double power_acc[NUM_CAMBRIS * 8] = {};
	static int sample_counter = 0;

	static int next_second = -1;
	if (next_second < 0) next_second = (int) time + 1;

	if (time > next_second) {

		FILE* f = fopen("status.log", "w");
		fprintf(f, "%s", format_timestamp(next_second));
		fprintf(f, "energy:%.1f", cambri_get_energy());
		fprintf(f, "\n");
		fclose(f);


		while (time > next_second) {
			fprintf(cambri_log_file, "%s", format_timestamp(next_second));
			next_second++;
			for (i = 0; i < NUM_CAMBRIS * 8; i++) {
				double power = power_acc[i] / sample_counter;
				energy += power;
				fprintf(cambri_log_file, " | %4.2f", power);
			}
			fprintf(cambri_log_file, "\n");
		}

		fflush(cambri_log_file);


		for (i = 0; i < NUM_CAMBRIS * 8; i++) power_acc[i] = 0;
		sample_counter = 0;
	}
	sample_counter++;



	int c;
	for (c = 0; c < NUM_CAMBRIS; c++) {
		if (!cambri_fds[c]) continue;
		char buf[4096] = {};


		// voltage
		cambri_write(c, "health");
		int ret = cambri_read(c, buf, sizeof(buf));
		if (ret == 0) error(1, 0, "cambri_log_power");
		char* p = strstr(buf, "5V Now: ");
		if (!p) error(1, 0, "cambri_log_power");
		double voltage;
		sscanf(p + 8, " %lf", &voltage);

		// current
		cambri_write(c, "state");
		ret = cambri_read(c, buf, sizeof(buf));
		if (ret == 0) error(1, 0, "cambri_log_power");
		p = buf;

		for (i = 0; i < 8; i++) {
			p = strchr(p, '\n');
			if (!p) error(1, 0, "cambri_log_power");
			int current = atoi(p += 5) * 0.001;
			int id = c * 8 + i;
			power_acc[id] += current * voltage;
			current_cache[id] = current;
		}
	}
}


void cambri_set_mode(int id, int mode) {
	int c = id / 1000 - 1;
	if (c < 0 || c >= NUM_CAMBRIS || !cambri_fds[c]) return;
	cambri_write(c, "mode %c %d 4", mode, id % 10);
	char buf[1024] = {};
	cambri_read(c, buf, sizeof(buf));
}

