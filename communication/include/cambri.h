#pragma once


enum {
	CAMBRI_OFF = 'o',
	CAMBRI_CHARGE = 'c'
};


int		cambri_init(void);
void	cambri_kill(void);
void	cambri_log_current(const char* time);
void	cambri_set_mode(int id, int mode);
