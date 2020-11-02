EMACS ?= emacs			# inherit environment variable EMACS

.PHONY: json
json: describe-symbol-aggregator.elc
	$(EMACS) -Q --batch -L . -l describe-symbol-aggregator -f describe-symbol-aggregator

%.elc: %.el
	$(EMACS) -Q --batch --eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile $<

.PHONY: car
car:
	$(EMACS) -Q --batch -L . -l describe-symbol-aggregator --eval "(describe-symbol-aggregator nil '(car alist-get emacs-version auth-source-backend))"

.PHONY: clean
clean:
	rm -f *.elc

