all:
	gcc -Wall -fPIC libalsavolume.c -c -o libalsavolume.o
	gcc -shared -Wl,-soname,libalsavolume.so -o libalsavolume.so libalsavolume.o

install:
	cp libalsavolume.so ${HOME}/lib/
	cp volume-control.lisp ${HOME}/bin/volume-control.lisp

clean:
	rm *.o *.so
