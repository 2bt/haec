#include <stdio.h>
#include <stdlib.h>
#include <error.h>

#include <racr/racr.h>

#include "server.h"
#include "event.h"


static Scheme_Object* eval_script(Scheme_Env* env, const char* filename) {
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


static Scheme_Object* prim_timestamp(int argc, Scheme_Object** argv) {
	return scheme_make_double(timestamp());
}


int main(int argc, char** argv) {

	RACR_INIT(env, "bytecode", NULL);

	scheme_add_global(
		"timestamp",
		scheme_make_prim_w_arity(prim_timestamp, "timestamp", 0, 0),
		env);

	eval_script(env, "scheme.scm");

	server_run();

	return 0;
}

