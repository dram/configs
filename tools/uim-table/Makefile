OUTFILE := zm-clean.table

all: $(OUTFILE)

$(OUTFILE): condense.py
	./condense.py $@

.PHONY: clean install

clean:
	rm -f *.o $(OUTFILE)

install: $(OUTFILE)
	sudo install -m 0644 ./zm-clean.table /usr/share/uim/tables/zhengma.table

