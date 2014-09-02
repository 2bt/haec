#pragma once

void cambri_init(void);
void cambri_kill(void);
void cambri_write(const char* fmt, ...);
int cambri_read(char* buf, int len);
