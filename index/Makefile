
all: index

index: Makefile obj/main.o obj/hook.o
	gcc -Wall -pthread -lJudy -lrt obj/main.o obj/hook.o -o $@

index-unsafe: Makefile obj/main.o
	gcc -Wall -pthread -lJudy -lrt obj/main.o -o $@

obj/hook.o: Makefile src/hook.c
	@mkdir -p obj
	@# important: disable optimization
	gcc -Wall -Wno-deprecated-declarations -O0 src/hook.c -c -o $@

obj/main.o: Makefile src/main.c
	@mkdir -p obj
	gcc -Wall -O2 src/main.c -c -o $@

clean:
	rm -rf obj index

