CC=clang
CFLAGS=-O3 -DNDEBUG -DCLANG
DEPS=common.h header.h nursery.h objects.h semispace.h stats.h utils.h \
		 marksweep.h
SHARED=stats.o header.o nursery.o utils.o objects.o semispace.o \
       lib_bintree.o lib_shadowstack.o marksweep.o

%.o: %.c $(DEPS)
	$(CC) $(CFLAGS) -c -o $@ $<

%.s: %.c $(DEPS)
	$(CC) $(CFLAGS) -S $<

%.ll: %.c $(DEPS)
	$(CC) $(CFLAGS) -S -emit-llvm $<

bench: benchmark.o $(SHARED)
	$(CC) -o bench benchmark.o $(SHARED)

test: FORCE
	@$(CC) -lcriterion -o test test.c $(SHARED:.o=.c)
	@./test --tap

FORCE:

clean:
	rm -f ./*.o ./bench ./test
