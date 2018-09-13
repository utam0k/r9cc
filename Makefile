r9cc:
	cargo build

test: r9cc  
	@gcc -E -P test/test.c > tmp-test.tmp
	@./target/debug/r9cc tmp-test.tmp > tmp-test.s
	@echo 'int global_arr[1] = {5};' | gcc -xc -c -o tmp-test2.o -
	@gcc -static -o tmp-test tmp-test.s tmp-test2.o
	@./tmp-test

clean:
	rm -f *~ tmp*

fib: r9cc
	@./target/debug/r9cc "$$(cat examples/fib.c)" > tmp-fib.s
	@gcc -static -o tmp-fib tmp-fib.s 
	@./tmp-fib

.PHONY: test clean
