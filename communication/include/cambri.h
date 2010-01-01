#pragma once


enum {
	CAMBRI_OFF = 'o',
	CAMBRI_CHARGE = 'c'
};


int		cambri_init(void);
void	cambri_kill(void);
void	cambri_write(int c, const char* fmt, ...);
int		cambri_read(int c, char* buf, int len);
void	cambri_log_current(double time);
void	cambri_set_mode(int id, int mode);

double  cambri_get_voltage(int cambri);

int		cambri_get_current_integral(void);
void	cambri_set_current_integral(int c);
