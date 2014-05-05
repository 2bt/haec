
all: index

index: Makefile obj/main.o
	gcc -Wall -pthread -lJudy -lrt obj/main.o -o $@

obj/main.o: Makefile src/main.c
	@mkdir -p obj
	gcc -Wall -O2 src/main.c -c -o $@

clean:
	rm -rf obj index

.PHONY: clean
