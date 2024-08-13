all: vm.o value.o
.PHONY: all

vm.o: vm.c vm.h value.h opcode.h
	$(CC) -c -o $@ -Wall $<

value.o: value.c vm.h value.h opcode.h
	$(CC) -c -o $@ -Wall $<
