#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <time.h>
#include <pthread.h>
#include <Judy.h>



enum {
	NUM_THREADS = 1,
	MAX_KEY_LEN = 256
};




int isWS(char c) {
	return c < '!' || c == ']' || c == '|' || c == '['
	|| c == '=' || c == '\"' || c == '\'' || c == '<'
	|| c == '>' || c == '{' || c == '}' || c == '(' || c == ')';
}


void* map(char* page) {
	void* table = NULL;
	char* p = page;
	while (*p && isWS(*p)) p++;
	while (*p) {
		char* start = p;
		while (*p && !isWS(*p)) p++;
		int len = p - start;
		while (*p && isWS(*p)) *p++ = '\0';
		if (len < MAX_KEY_LEN - 1) {
			int* value;
			JSLI(value, table, (uint8_t*) start);
			*value += 1;
		}
	}
	return table;
}


void* reduce(void* ta, void* tb) {
	int* vb;
	int* va;
	uint8_t key[MAX_KEY_LEN] = "";
	JSLF(vb, tb, key);
	while (vb) {
		JSLI(va, ta, key);
		*va += *vb;
		JSLN(vb, tb, key);
	}
	return ta;
}


void print_table(void* table) {
	uint8_t key[MAX_KEY_LEN] = "";
	int* value;
	JSLF(value, table, key);
	while (value) {
		printf("%8d %s\n", *value, key);
		JSLN(value, table, key);
	}
}


FILE* pages_file;

pthread_mutex_t	pages_mutex;
pthread_mutex_t	tables_mutex;
pthread_cond_t	tables_cond;


enum { BUFFER_MAX = 1 << 20 };
char buffer[BUFFER_MAX];
int buffer_pos = BUFFER_MAX;
int buffer_size = BUFFER_MAX;

int next_char(void) {
	if (buffer_pos == buffer_size) {
		if (buffer_size < BUFFER_MAX) return EOF;
		buffer_size = fread(buffer, 1, BUFFER_MAX, pages_file);
		buffer_pos = 0;
	}
	return buffer[buffer_pos++];
}


char* next_page(void) {

	pthread_mutex_lock(&pages_mutex);

	int c = next_char();
	if (c == EOF) {
		pthread_mutex_unlock(&pages_mutex);
		return NULL;
	}

	int		size = 1024;
	char*	page = malloc(size);
	int		i = 0;
	for (;;) {
		if (c == '\0' || c == EOF) {
			page[i] = '\0';
			pthread_mutex_unlock(&pages_mutex);
			return page;
		}
		page[i] = c;
		i++;
		if (i >= size) {
			size *= 2;
			page = realloc(page, size);
		}
		c = next_char();
	}
}


typedef struct TableList {
	void* table;
	struct TableList* next;
} TableList;
TableList*	tables;


void push_table(void* table) {
	pthread_mutex_lock(&tables_mutex);
	TableList* n = malloc(sizeof(TableList));
	n->next = tables;
	n->table = table;
	tables = n;
	pthread_cond_signal(&tables_cond);
	pthread_mutex_unlock(&tables_mutex);
}

void* pop_table(void) {
	if (!tables) return NULL;
	pthread_mutex_lock(&tables_mutex);
	TableList* n = tables;
	tables = n->next;
	void* table = n->table;
	free(n);
	pthread_mutex_unlock(&tables_mutex);
	return table;
}



void single_thread_map(void) {
	char* page;
	while ((page = next_page())) {
		void* table = map(page);
		free(page);
		push_table(table);
	}
}


void* single_thread_map_reduce(void) {
	char* page;
	while ((page = next_page())) {
		void* table = map(page);
		free(page);
		push_table(table);
	}

	void* table = NULL;
	void* table_b;
	while ((table_b = pop_table())) {
		if (!table) table = table_b;
		else {
			table = reduce(table, table_b);
			int size;
			JSLFA(size, table_b);
		}
	}

	return table;
}



int workers_working = NUM_THREADS;

void* worker_map(void* arg) {
	char* page;
	while ((page = next_page())) {
		void* table = map(page);
		free(page);
		push_table(table);
	}
	pthread_mutex_lock(&tables_mutex);
	workers_working--;
	pthread_mutex_unlock(&tables_mutex);
	return NULL;
}

void multi_thread_map(void) {

	pthread_mutex_init(&pages_mutex, NULL);
	pthread_mutex_init(&tables_mutex, NULL);

	pthread_t threads[NUM_THREADS];
	int i;
	for (i = 0; i < NUM_THREADS; i++) {
		pthread_create(&threads[i], NULL, worker_map, NULL);
	}

	worker_map(NULL);

	for (i = 0; i < NUM_THREADS; i++) {
		pthread_join(threads[i], NULL);
	}

	pthread_mutex_destroy(&pages_mutex);
	pthread_mutex_destroy(&tables_mutex);

}



void* multi_thread_map_reduce(void) {

	pthread_mutex_init(&pages_mutex, NULL);
	pthread_mutex_init(&tables_mutex, NULL);
	pthread_cond_init(&tables_cond, NULL);

	pthread_t threads[NUM_THREADS];
	int i;
	for (i = 0; i < NUM_THREADS; i++) {
		pthread_create(&threads[i], NULL, worker_map, NULL);
	}

	void* table = NULL;
	pthread_mutex_lock(&tables_mutex);
	while (workers_working || tables) {
		pthread_cond_wait(&tables_cond, &tables_mutex);
		pthread_mutex_unlock(&tables_mutex);

		void* table_b;
		while ((table_b = pop_table())) {
			if (!table) table = table_b;
			else {
				table = reduce(table, table_b);
				int size;
				JSLFA(size, table_b);
			}
		}

		pthread_mutex_lock(&tables_mutex);
	}
	pthread_mutex_unlock(&tables_mutex);

	for (i = 0; i < NUM_THREADS; i++) {
		pthread_join(threads[i], NULL);
	}

	pthread_mutex_destroy(&pages_mutex);
	pthread_mutex_destroy(&tables_mutex);
	pthread_cond_destroy(&tables_cond);

	return table;
}




void usage(int argc, char** argv) {
	printf("usage: %s [ m | mr | tm | tmr ] filename\n", argv[0]);
	exit(0);
}


int main(int argc, char** argv) {

	if (argc != 3) usage(argc, argv);

	pages_file = fopen(argv[2], "r");
	if (!pages_file) {
		fprintf(stderr, "error opening file %s\n", argv[2]);
		exit(1);
	}

	struct timespec t1, t2;
	clock_gettime(CLOCK_REALTIME, &t1);

	if (strcmp(argv[1], "m") == 0) {
		single_thread_map();
		void* t;
		while ((t = pop_table())) {
			int size;
			JSLFA(size, t);
		}
	}
	else if (strcmp(argv[1], "tm") == 0) {
		multi_thread_map();
		void* t;
		while ((t = pop_table())) {
			int size;
			JSLFA(size, t);
		}
	}
	else if (strcmp(argv[1], "mr") == 0) {
		void* table = single_thread_map_reduce();
//		print_table(table);
		int size;
		JSLFA(size, table);
	}
	else if (strcmp(argv[1], "tmr") == 0) {
		void* table = multi_thread_map_reduce();
//		print_table(table);
		int size;
		JSLFA(size, table);
	}
	else usage(argc, argv);

	fclose(pages_file);


	clock_gettime(CLOCK_REALTIME, &t2);
	long mseconds = (t2.tv_nsec - t1.tv_nsec) / 1000000 + (t2.tv_sec - t1.tv_sec) * 1000;
	printf("%ld.%03ld\n", mseconds / 1000, mseconds % 1000);

	return 0;
}


