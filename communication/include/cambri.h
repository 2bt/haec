#pragma once


enum { NUM_CAMBRIS = 4 };


enum {
	CAMBRI_OFF = 'o',
	CAMBRI_CHARGE = 'c'
};


int		cambri_init(void);
void	cambri_kill(void);
void	cambri_write(int c, const char* fmt, ...);
int		cambri_read(int c, char* buf, int len);
void	cambri_log_data(double time, char* scheduler);
void	cambri_set_mode(int id, int mode);

int		cambri_get_current(int id);
double	cambri_get_energy(void);
void	cambri_set_energy(double e);
