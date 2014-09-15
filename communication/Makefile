RACKET_PATH = /home/twobit/Programming/scheme/racket
RACR_PATH = /home/twobit/Programming/scheme/racr

CRACR = -I$(RACR_PATH)/racr-c/include -I$(RACKET_PATH)/racket/include
LRACR = -lm -lpthread -ldl -lffi $(RACR_PATH)/racr-c/build/racr.o
LRACR += $(RACKET_PATH)/racket/src/build/racket/libracket.a
LRACR += $(RACKET_PATH)/racket/src/build/racket/libmzgc.a


CF = -Wall -O2 -Iinclude
LF = -pthread

all: server worker


SERVERSRC = src/main.c src/server.c src/cambri.c src/event.c src/worker.c
SERVEROBJ = $(SERVERSRC:src/%.c=obj/%.o)

server: Makefile $(SERVEROBJ)
	gcc $(LF) $(LRACR) $(SERVEROBJ) -o $@


obj/%.o: src/%.c Makefile
	@mkdir -p obj
	gcc $(CF) $(CRACR) $< -c -o $@




worker: Makefile obj/worker_main.o
	gcc $(LF) obj/worker_main.o -o $@

obj/worker_main.o: Makefile src/worker_main.c
	@mkdir -p obj
	gcc $(CF) src/worker_main.c -c -o $@



bytecode:
	@WD="$$PWD" &&\
	cd .. &&\
	cd $(RACR_PATH)/racr-c && ./bin/make_bc "`echo $$WD/bytecode`"

clean:
	rm -rf obj server worker
	rm server.log cambri.log

.PHONY: clean bytecode
