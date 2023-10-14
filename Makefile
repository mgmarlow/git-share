EMACS := emacs -Q -batch

.PHONY: build test clean

build: clean
	$(EMACS) -L . -f batch-byte-compile git-share.el

test: build
	$(EMACS) -l ert -L . -l git-share-test.el -f ert-run-tests-batch-and-exit

clean:
	rm -f git-share.elc
