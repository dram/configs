OUTFILE := zhengma-rule.scm

all: $(OUTFILE)

$(OUTFILE): condense.py table-to-rules.py
	./condense.py zm-clean.table
	./table-to-rules.py zm-clean.table $@

.PHONY: clean install

clean:
	rm -f *.o $(OUTFILE)

install: $(OUTFILE)
	install -m 0644 $< /usr/share/uim/$<
