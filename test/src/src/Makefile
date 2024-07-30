tests := $(shell seq -f "%02.0f" 0 74)
fun_c := $(patsubst %,../f%.c,$(tests))

all : $(fun_c)

$(fun_c) : ../f%.c : fun.c
	gcc -P -E $< -DTEST_NO=$(shell seq $* $*) -o $@

$(fun_s) : ../f%.s : f%.c
	gcc -O3 -S $< -o $@

.DELETE_ON_ERROR:
