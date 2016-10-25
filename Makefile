emacs ?= emacs
wget ?= wget
ruby ?= ruby

.PHONY: test
all: test
test:
	$(emacs) -Q -batch -L . -l ert -l test/company-makefile-tests.el \
	-f ert-run-tests-batch-and-exit

README.md: el2markdown.el company-makefile.el
	$(emacs) -batch -l $< company-makefile.el -f el2markdown-write-readme
	$(RM) $@~

.INTERMEDIATE: el2markdown.el
el2markdown.el:
	$(wget) -q -O $@ "https://github.com/Lindydancer/el2markdown/raw/master/el2markdown.el"

impvars.dat: build/impvars.json
	$(emacs) -batch -l build/vars.el -f batch-convert $<

.INTERMEDIATE: build/impvars.json
build/impvars.json: build/vars.rb build/vars.el
	$(ruby) build/vars.rb
