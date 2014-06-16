#include <stdio.h>
#include <stdlib.h>
#include <error.h>

#include <racr/racr.h>

#include "server.h"
#include "cambri.h"



Scheme_Object* eval_script(Scheme_Env* env, const char* filename) {
	FILE* f = fopen(filename, "r");
	if (!f) error(1, 0, "script");
	fseek(f, 0, SEEK_END);
	long size = ftell(f);
	rewind(f);
	char source[size + 1];
	fread(source, 1, size, f);
	source[size] = '\0';
	return scheme_eval_string_all(source, env, 1);
}




int main(int argc, char** argv) {

	RACR_INIT(env, "bytecode", NULL);
	eval_script(env, "scheme.scm");

	//cambri_init();

	server_run();

	cambri_kill();

	return 0;
}

