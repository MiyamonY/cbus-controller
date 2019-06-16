.PHONY: test clean

run:
	racket main.rkt

test:
	gcc -shared -I./test test/libftd2xx_test.c -o libtestftd2xx.so
	raco test .

clean:
	- $(RM) -f libtestftd2xx.so
