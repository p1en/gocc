build:
	go build .

test:
	./gocc tests.c > tmp.s
	gcc -static -o tmp tmp.s -z noexecstack
	./tmp

clean:
	rm -f gocc tmp*
