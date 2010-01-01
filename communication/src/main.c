#include <stdio.h>
#include <stdlib.h>
#include <error.h>

#include <racr/racr.h>

#include "server.h"
#include "event.h"


static Scheme_Env* global_env;


static Scheme_Object* eval_script(const char* filename) {
	FILE* f = fopen(filename, "r");
	if (!f) error(1, 0, "script");
	fseek(f, 0, SEEK_END);
	long size = ftell(f);
	rewind(f);
	char source[size + 1];
	fread(source, 1, size, f);
	source[size] = '\0';
	return scheme_eval_string_all(source, global_env, 1);
}


static Scheme_Object* prim_timestamp(int argc, Scheme_Object** argv) {
	return scheme_make_double(timestamp());
}


static Scheme_Object* prim_add_event(int argc, Scheme_Object** argv) {
	if (!SCHEME_SYMBOLP(argv[0])) error(1, 0, "prim_add_event");
	const char* sym = SCHEME_SYM_VAL(argv[0]);

	if (strcmp(sym, "event-work-command") == 0) {
		Event* e = event_append(EVENT_WORK_COMMAND);
		e->worker = worker_find_by_id(SCHEME_INT_VAL(argv[1]));
		e->work_id = SCHEME_INT_VAL(argv[2]);
		e->load_size = SCHEME_DBL_VAL(argv[3]);
		return scheme_true;
	}
	else if (strcmp(sym, "event-halt-command") == 0) {
		Event* e = event_append(EVENT_HALT_COMMAND);
		e->worker = worker_find_by_id(SCHEME_INT_VAL(argv[1]));
		return scheme_true;
	}
	else if (strcmp(sym, "event-worker-on") == 0) {
		Event* e = event_append(EVENT_WORKER_ON);
		e->worker = worker_find_by_id(SCHEME_INT_VAL(argv[1]));
		return scheme_true;
	}
	else if (strcmp(sym, "event-switch-on") == 0) {
		Event* e = event_append(EVENT_SWITCH_ON);
		e->worker = worker_find_by_id(SCHEME_INT_VAL(argv[1]));
		return scheme_true;
	}
	else if (strcmp(sym, "event-switch-off") == 0) {
		Event* e = event_append(EVENT_SWITCH_OFF);
		e->worker = worker_find_by_id(SCHEME_INT_VAL(argv[1]));
		return scheme_true;
	}

	return scheme_false;
}


int eval_string(const char* str) {

	Scheme_Config* config = scheme_current_config();
	Scheme_Object* curout = scheme_get_param(config, MZCONFIG_OUTPUT_PORT);

	mz_jmp_buf fresh;
	mz_jmp_buf* volatile save = scheme_current_thread->error_buf;
	scheme_current_thread->error_buf = &fresh;
	if (scheme_setjmp(scheme_error_buf)) {
		scheme_current_thread->error_buf = save;
		return -1;
	}
	else {
		Scheme_Object* v = scheme_eval_string_all(str, global_env, 1);
		scheme_display(v, curout);
		scheme_display(scheme_make_char('\n'), curout);
		scheme_current_thread->error_buf = save;
	}

	return 0;
}


int main(int argc, char** argv) {

	RACR_INIT(env, "bytecode", NULL);

	global_env = env;
	scheme_add_global(
		"timestamp",
		scheme_make_prim_w_arity(prim_timestamp, "timestamp", 0, 0),
		env);

	scheme_add_global(
		"add-event",
		scheme_make_prim_w_arity(prim_add_event, "add-event", 1, 5),
		env);

	eval_script("scheme.scm");

	server_run(argc, argv);

	return 0;
}

