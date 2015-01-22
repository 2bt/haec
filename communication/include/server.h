#pragma once


enum {
	STDIN = 0,
	PORT = 1337
};


typedef struct {
	int		running;
	int		work_counter;
	double	timestamp;
	double	e_meter_timestamp;

	fd_set	fds;
	int		fdmax;

	FILE*	log_fd;
	FILE*	scenario_fd;
	double	scenario_timestamp;
	char	scenario_cmd[256];
	double	scenario_cmd_time;
} Server;

extern Server server;

void server_run(int argc, char** argv);
void server_log_event(const char* fmt, ...);
