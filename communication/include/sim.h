#pragma once

#ifdef SIM
	#define		select				sim_select
	#define		absolute_timestamp	sim_absolute_timestamp

	#define		cambri_init    		sim_cambri_init
	#define		cambri_kill    		sim_cambri_kill
	#define		cambri_write   		sim_cambri_write
	#define		cambri_read    		sim_cambri_read
	#define		cambri_log_data		sim_cambri_log_data
	#define		cambri_set_mode		sim_cambri_set_mode


	#define		racr_call_str(...)	do { sim_racr_time_start(); racr_call_str(__VA_ARGS__); sim_racr_time_end(); } while(0);

#endif



double	absolute_timestamp(void);
int		select(int nfds, fd_set *readfds, fd_set *writefds, fd_set *exceptfds, struct timeval *timeout);

int		sim_cambri_init(void);
void	sim_cambri_kill(void);
void	sim_cambri_write(int c, const char* fmt, ...);
int		sim_cambri_read(int c, char* buf, int len);
void	sim_cambri_log_data(double time, char* scheduler);
void	sim_cambri_set_mode(int id, int mode);

void	sim_racr_time_start(void);
void	sim_racr_time_end(void);
