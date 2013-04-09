PROG=emacs-sunpinyin

OBJS=main.o

CPPFLAGS=`pkg-config --cflags sunpinyin-2.0`
LDFLAGS=`pkg-config --libs sunpinyin-2.0`


${PROG}: ${OBJS}
	g++ ${LDFLAGS} $^ -o $@

main.o: main.cpp


.PHONY: clean

clean:
	rm ${PROG} ${OBJS}

