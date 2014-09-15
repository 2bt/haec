#pragma once


enum {
	CAMBRI_OFF = 'o',
	CAMBRI_CHARGE = 'c'
};


int		cambri_init(void);
void	cambri_kill(void);
void	cambri_write(const char* fmt, ...);
int		cambri_read(char* buf, int len);
void	cambri_log_current(const char* time);
void	cambri_set_mode(int id, int mode);
