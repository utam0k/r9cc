r9cc = ./target/debug/r9cc

build:
	cargo build

test: build
	@gcc -E -P test/test.c | $(r9cc) - > tmp-test1.s
	@$(r9cc) ./test/token.c > tmp-test2.s
	@echo 'int global_arr[1] = {5};' | gcc -xc -c -o tmp-test2.o -
	@gcc -static -o tmp-test tmp-test1.s tmp-test2.s tmp-test2.o
	@./tmp-test

clean:
	rm -f *~ tmp*

fib:
	@$(r9cc) examples/fib.c > tmp-fib.s
	@gcc -static -o tmp-fib tmp-fib.s 
	@./tmp-fib

prime:
	@$(r9cc) examples/prime.c > tmp-prime.s
	@gcc -static -o tmp-prime tmp-prime.s
	@./tmp-prime

.PHONY: test clean
