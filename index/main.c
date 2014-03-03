#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <Judy.h>



enum {
	NUM_THREADS = 1,
	MAX_KEY_LEN = 1 << 12
};


int get_text(const char* filename, char** text) {
	FILE* file = fopen(filename, "r");
	if (!file) return -1;
	fseek(file, 0, SEEK_END);
	int size = ftell(file);
	fseek(file, 0, SEEK_SET);
	*text = malloc(size + 1);
	fread(*text, 1, size, file);
	(*text)[size] = '\0';
	return size;
}


int get_pages(char* text, int len, char*** pages) {
	int num_pages = 0;
	char** p = NULL;
	int i = 0;
	while (i < len) {
		num_pages++;
		p = realloc(p, sizeof(char*) * num_pages);
		p[num_pages - 1] = text + i;
		while (i < len && text[i] != '\0') i++;
		i++;
	}
	*pages = p;
	return num_pages;
}


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
		while (*p && isWS(*p)) *p++ = '\0';
		int* value;
		JSLI(value, table, (uint8_t*) start);
		*value += 1;
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


// single-threaded

void** single_thread_map(char** pages, int num_pages) {

	void** tables = malloc(sizeof(void*) * num_pages);
	for (int i = 0; i < num_pages; i++) {
		tables[i] = map(pages[i]);
	}

	return tables;
}


void* single_thread_map_reduce(char** pages, int num_pages) {

	void** tables = malloc(sizeof(void*) * num_pages);
	for (int i = 0; i < num_pages; i++) {
		tables[i] = map(pages[i]);
	}

	void* table = tables[0];
	for (int i = 1; i < num_pages; i++) {
		reduce(table, tables[i]);
		int size;
		JSLFA(size, tables[i]);
	}
	free(tables);

	return table;
}



// multi-threaded

char**	g_pages;
int		g_num_pages;
int		g_current_page;

void**	g_tables;
int		g_current_table;

pthread_mutex_t	pages_mutex;
pthread_mutex_t	tables_mutex;
pthread_cond_t	tables_cond;

char* next_page(void) {
	char* res = NULL;
	pthread_mutex_lock(&pages_mutex);
	if (g_current_page < g_num_pages) {
		res = g_pages[g_current_page];
		g_current_page++;
	}
	pthread_mutex_unlock(&pages_mutex);
	return res;
}

void add_table(void* table) {
	pthread_mutex_lock(&tables_mutex);
	g_tables[g_current_table] = table;
	g_current_table++;

	pthread_cond_signal(&tables_cond);

	pthread_mutex_unlock(&tables_mutex);
}

void* worker_map(void* arg) {
	char* page;
	while ((page = next_page())) add_table(map(page));
	pthread_exit(NULL);
}

void** multi_thread_map(char** pages, int num_pages) {
	g_num_pages = num_pages;
	g_pages = pages;
	g_current_page = 0;

	g_tables = calloc(g_num_pages, sizeof(void*));
	g_current_table = 0;

	pthread_mutex_init(&pages_mutex, NULL);
	pthread_mutex_init(&tables_mutex, NULL);

	pthread_t threads[NUM_THREADS];
	for (int i = 0; i < NUM_THREADS; i++) {
		pthread_create(&threads[i], NULL, worker_map, NULL);
	}

	worker_map(NULL);

	for (int i = 0; i < NUM_THREADS; i++) {
		pthread_join(threads[i], NULL);
	}

	pthread_mutex_destroy(&pages_mutex);
	pthread_mutex_destroy(&tables_mutex);

	return g_tables;
}



void* multi_thread_map_reduce(char** pages, int num_pages) {
	g_num_pages = num_pages;
	g_pages = pages;
	g_current_page = 0;

	g_tables = calloc(g_num_pages, sizeof(void*));
	g_current_table = 0;

	pthread_mutex_init(&pages_mutex, NULL);
	pthread_mutex_init(&tables_mutex, NULL);
	pthread_cond_init(&tables_cond, NULL);

	pthread_t threads[NUM_THREADS];
	for (int i = 0; i < NUM_THREADS; i++) {
		pthread_create(&threads[i], NULL, worker_map, NULL);
	}

	void* table = NULL;
	int i = 0;
	pthread_mutex_lock(&tables_mutex);
	while (i < g_num_pages) {
		pthread_cond_wait(&tables_cond, &tables_mutex);
		pthread_mutex_unlock(&tables_mutex);

		while (i < g_current_table) {
			if (i == 0) table = g_tables[0];
			else {
				reduce(table, g_tables[i]);
				int size;
				JSLFA(size, g_tables[i]);
			}
			i++;
		}

		pthread_mutex_lock(&tables_mutex);
	}
	pthread_mutex_unlock(&tables_mutex);

	for (int i = 0; i < NUM_THREADS; i++) {
		pthread_join(threads[i], NULL);
	}

	pthread_mutex_destroy(&pages_mutex);
	pthread_mutex_destroy(&tables_mutex);
	pthread_cond_destroy(&tables_cond);

	free(g_tables);
	return table;
}









void usage(int argc, char** argv) {
	printf("usage: %s [ m | mr | tm | tmr ] filename\n", argv[0]);
	exit(0);
}


int main(int argc, char** argv) {
	if (argc != 3) usage(argc, argv);

	char* text;
	int text_len = get_text(argv[2], &text);
	if (text_len <= 0) {
		fprintf(stderr, "error opening file %s\n", argv[2]);
		exit(1);
	}

	char** pages;
	int num_pages = get_pages(text, text_len, &pages);

	if (strcmp(argv[1], "m") == 0) {
		void** tables = single_thread_map(pages, num_pages);
		for (int i = 0; i < num_pages; i++) {
			int size;
			JSLFA(size, tables[i]);
		}
		free(tables);
	}
	else if (strcmp(argv[1], "tm") == 0) {
		void** tables = multi_thread_map(pages, num_pages);
		for (int i = 0; i < num_pages; i++) {
			int size;
			JSLFA(size, tables[i]);
		}
		free(tables);
	}
	else if (strcmp(argv[1], "mr") == 0) {
		void* table = single_thread_map_reduce(pages, num_pages);
//		print_table(table);
		int size;
		JSLFA(size, table);
	}
	else if (strcmp(argv[1], "tmr") == 0) {
		void* table = multi_thread_map_reduce(pages, num_pages);
//		print_table(table);
		int size;
		JSLFA(size, table);
	}
	else usage(argc, argv);


	free(text);
	free(pages);

	return 0;
}


