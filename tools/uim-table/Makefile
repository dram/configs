all: zhengma-rule.scm zhengma.el

zhengma-rule.scm: condense.py table-to-rules.py
	./condense.py zm-clean.table
	./table-to-rules.py zm-clean.table $@

zhengma.el:
	./table-to-emacs-rules.py zm-clean.table $@

.PHONY: clean install

clean:
	rm -f *.o zhengma-rule.scm zhengma.el

install: zhengma-rule.scm
	install -m 0644 $< /usr/share/uim/$<
