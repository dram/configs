PROG=fake-key

CFLAGS=-Wall

LDFLAGS=-lX11 -lXtst

$(PROG): $(PROG).o
	gcc -o $@ $(LDFLAGS) $^

$(PROG).o: $(PROG).c

.PHONY: clean

clean:
	rm -f $(PROG) *.o
