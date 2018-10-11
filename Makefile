r9cc = ./target/debug/r9cc

build:
	cargo build

test: build
	@gcc -E -P test/test.c | $(r9cc) - > tmp-test1.s
	@gcc -c -o tmp-test2.o test/gcc.c
	@gcc -static -o tmp-test1 tmp-test1.s tmp-test2.o
	@./tmp-test1
	@$(r9cc) ./test/token.c > tmp-test2.s
	@gcc -static -o tmp-test2 tmp-test2.s
	@./tmp-test2

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
