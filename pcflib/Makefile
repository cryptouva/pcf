all: pcflib.o test

pcflib.o: pcflib.c
	gcc -fPIC pcflib.c -c -Wall -Werror -g

test: pcflib.o test.c
	gcc -fPIC -o test test.c pcflib.o -Wall -Werror -g
