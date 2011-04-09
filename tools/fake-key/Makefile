PROG=fake-key

CFLAGS=-Wall

LDFLAGS=-lXtst

$(PROG): $(PROG).o
	gcc -o $@ $(LDFLAGS) $^

$(PROG).o: $(PROG).c

.PHONY: clean

clean:
	rm -f $(PROG) *.o
