emacs ?= emacs
wget  ?= wget
ruby  ?= ruby

.PHONY: test help all
all: help

test:  ## Run tests
	$(emacs) -Q -batch -L . -l ert -l test/company-makefile-tests.el \
		-f ert-run-tests-batch-and-exit

README.md: el2markdown.el company-makefile.el  ## Create README.md
	$(emacs) -batch -l $< company-makefile.el -f el2markdown-write-readme
	$(RM) $@~

.INTERMEDIATE: el2markdown.el
el2markdown.el:  ## Download el2markdown.el converter
	$(wget) -q -O $@                                                 \
	"https://github.com/Lindydancer/el2markdown/raw/master/el2markdown.el"

impvars.el: build/impvars.json  ## Get implicit variable info
	$(emacs) -batch -l build/vars.el -f batch-convert $<

defaults.el: build/defaults ## Get default make values from local make
	@(cd build && ./$(<F))

.INTERMEDIATE: build/impvars.json
build/impvars.json: build/vars.rb build/vars.el ## Implicit variable info to JSON
	$(ruby) build/vars.rb

help:  ## Show help
	@grep -E '^[/.%a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) |         \
	sort | awk                                                       \
	'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
