PROG=emacs-fcitx

OBJS=main.o

CFLAGS=-Wall `pkg-config --cflags glib-2.0 gio-unix-2.0`
LDFLAGS=`pkg-config --libs gio-unix-2.0 fcitx-gclient`

${PROG}: ${OBJS}
	gcc ${LDFLAGS} $^ -o $@

main.o: main.c

.PHONY: clean

clean:
	rm ${PROG} ${OBJS}
