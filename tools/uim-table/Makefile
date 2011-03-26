OUTFILE := zm-clean.table

all: $(OUTFILE)

$(OUTFILE): condense.py
	./condense.py $@

.PHONY: clean install

clean:
	rm -f *.o $(OUTFILE)

install: $(OUTFILE)
	sudo cp ./zm-clean.table /usr/share/uim/tables/zhengma.table

