CXX_CFLAGS = -I$(HOME)/lccyao2/pcflib/ -I$(HOME)/include/ -I$(HOME)/local/include -g -L $(HOME)/lib -L$(HOME)/local/lib -Wno-deprecated -D__STDC_LIMIT_MACROS -DFREE_XOR -DRAND_SEED -DGRR  -D_BETTERYAO -DBETTERYAO -O0
CXX        = g++
MPI_CXX    = mpicxx

LIBS       = -lpbc -lgmp -lcrypto -llog4cxx -lpthread
HEADERS    = Algebra.h Bytes.h Circuit.h Env.h garbled_circuit.h NetIO.h Prng.h ClawFree.h 
OBJS       = Algebra.o Bytes.o Circuit.o Env.o garbled_circuit.o NetIO.o Prng.o Aes.o pcflib.o opdefs.o \
	garbled_circuit_m.o GarbledCct3.o
MAINS      = main.cpp YaoBase.cpp Yao.cpp BetterYao4.cpp

all : sim pcflib

betteryao: $(MAINS) $(OBJS)
	$(MPI_CXX) -o evl -DEVL_CODE $(CXX_CFLAGS) $^ $(LIBS)
	$(MPI_CXX) -o gen -DGEN_CODE $(CXX_CFLAGS) $^ $(LIBS)

pcflib: pcflib.o
	gcc -shared -fPIC ../pcflib.o -o pcflib.so

debug-test: debug
	gcc -o pcftest pcflib.o test.c -g -lpthread

debug: pcflib.c pcflib.h
	gcc -Wall -Werror -c -fPIC pcflib.c -g

pcflib.o: ../pcflib.c ../pcflib.h
	gcc -Wall -Werror -c -fPIC ../pcflib.c -g -DBETTERYAO

opdefs.o: ../opdefs.c ../pcflib.h ../opdefs.h 
	gcc -Wall -Werror -c -fPIC ../opdefs.c -g -DBETTERYAO

sim: $(MAINS) $(OBJS)
	$(MPI_CXX) -o sim $(CXX_CFLAGS) $^ $(LIBS)

test-circuit: test-circuit.cpp $(OBJS)
	$(MPI_CXX) $(CXX_CFLAGS) -o test-circuit $(CXX_LFLAGS) $^ $(LIBS)

server : ipserver.cpp Bytes.o Env.o NetIO.o
	$(CXX) -pthread -o server $(CXX_CFLAGS) $(CXX_LFLAGS) $^ $(LIBS)

GarbledCct.o : Algebra.h Bytes.h Circuit.h Env.h Prng.h GarbledCct.h GarbledCct.cpp
	$(CXX) -msse2 $(CXX_CFLAGS) -c GarbledCct.cpp

GarbledCct3.o : Algebra.h Bytes.h Circuit.h Env.h Prng.h GarbledCct3.h GarbledCct3.cpp
	$(CXX) -msse2 $(CXX_CFLAGS) -c GarbledCct3.cpp

garbled_circuit.o : Algebra.h Bytes.h Circuit.h Env.h Prng.h garbled_circuit.h garbled_circuit.cpp
	$(CXX) -msse2 $(CXX_CFLAGS) -c garbled_circuit.cpp

garbled_circuit_m.o : Algebra.h Bytes.h Circuit.h Env.h Prng.h garbled_circuit_m.h garbled_circuit_m.cpp
	$(CXX) -msse2 $(CXX_CFLAGS) -c garbled_circuit_m.cpp

Env.o : Algebra.h Bytes.h ClawFree.h Circuit.h NetIO.h Env.h Env.cpp 
	$(CXX) $(CXX_CFLAGS) -c Env.cpp

NetIO.o : Bytes.h NetIO.h NetIO.cpp
	$(CXX) $(CXX_CFLAGS) -c NetIO.cpp

Algebra.o: Bytes.h Prng.h Algebra.h Algebra.cpp
	$(CXX) $(CXX_CFLAGS) -c Algebra.cpp

Circuit.o : Bytes.h Circuit.h Circuit.cpp
	$(CXX) $(CXX_CFLAGS) -c Circuit.cpp

Prng.o: Bytes.h Prng.h Prng.cpp
	$(CXX) $(CXX_CFLAGS) -c Prng.cpp

Aes.o: Bytes.h Aes.cpp
	$(CXX) $(CXX_CFLAGS) -c Aes.cpp

Bytes.o: Bytes.h Hash.h Bytes.cpp
	$(CXX) $(CXX_CFLAGS) -c Bytes.cpp

clean :
	rm -f *.o gen evl sim test-circuit server
