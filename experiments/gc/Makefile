CC=clang
CFLAGS=-O3 -DNDEBUG
DEPS=common.h header.h nursery.h objects.h semispace.h stats.h utils.h
SHARED=stats.o header.o nursery.o utils.o objects.o semispace.o \
       lib_bintree.o lib_shadowstack.o

%.o: %.c $(DEPS)
	$(CC) $(CFLAGS) -c -o $@ $<

bench: benchmark.o $(SHARED)
	$(CC) -o bench benchmark.o $(SHARED)

test: test.c $(DEPS) $(SHARED:.o=.c)
	$(CC) -o test test.c $(SHARED:.o=.c)

clean:
	rm -f ./*.o ./bench ./test
