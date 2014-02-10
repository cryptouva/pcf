all: pcflib.o test

pcflib.o: pcflib.c pcflib.h opdefs.h
	gcc -fPIC pcflib.c -c -Wall -Werror -g

opdefs.o: opdefs.c opdefs.h pcflib.h
	gcc -fPIC opdefs.c -c -Wall -Werror -g

test: pcflib.o opdefs.o test.c
	gcc -fPIC -o test test.c pcflib.o opdefs.o -Wall -Werror -g

cirgen: pcflib.o opdefs.o cirgen.c
	gcc -fPIC -o cirgen cirgen.c pcflib.o opdefs.o -Wall -Werror -g