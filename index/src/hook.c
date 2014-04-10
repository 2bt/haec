#include <stdio.h>
#include <malloc.h>
#include <pthread.h>

static pthread_spinlock_t malloc_spin;

static void* (*old_malloc_hook)(size_t, const void*);
static void (*old_free_hook)(void*, const void*);
static void *my_malloc_hook (size_t, const void *);
static void my_free_hook (void*, const void *);

static void * my_malloc_hook(size_t size, const void *caller) {
	pthread_spin_lock(&malloc_spin);

	__malloc_hook = old_malloc_hook;
	__free_hook = old_free_hook;
	void* result = malloc (size);
	old_malloc_hook = __malloc_hook;
	old_free_hook = __free_hook;
	__malloc_hook = my_malloc_hook;
	__free_hook = my_free_hook;

	pthread_spin_unlock(&malloc_spin);
	return result;
}

static void my_free_hook(void *ptr, const void *caller) {
	pthread_spin_lock(&malloc_spin);

	__malloc_hook = old_malloc_hook;
	__free_hook = old_free_hook;
	free (ptr);
	old_malloc_hook = __malloc_hook;
	old_free_hook = __free_hook;
	__malloc_hook = my_malloc_hook;
	__free_hook = my_free_hook;

	pthread_spin_unlock(&malloc_spin);
}

static void my_init_hook(void) {
	pthread_spin_init(&malloc_spin, 0);

	old_malloc_hook = __malloc_hook;
	old_free_hook = __free_hook;
	__malloc_hook = my_malloc_hook;
	__free_hook = my_free_hook;
}

#ifdef __arm__
#define __MALLOC_HOOK_VOLATILE
#endif

void (*__MALLOC_HOOK_VOLATILE __malloc_initialize_hook)(void) = my_init_hook;

