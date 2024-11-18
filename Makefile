build:
	go build .

test:
	./gocc test/tests.c > tmp.s
	gcc -static -o tmp tmp.s -z noexecstack
	./tmp

clean:
	rm -f gocc tmp*

.PHONY: test
