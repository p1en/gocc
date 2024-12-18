build:
	go build .

test:
	./gocc test/tests.c > tmp.s
	echo 'int char_fn() { return 257; }' | gcc -xc -c -o tmp2.o -
	gcc -static -o tmp tmp.s tmp2.o -z noexecstack
	./tmp

clean:
	rm -f gocc tmp*

.PHONY: test
