EMACS = emacs

.PHONY: json
json: describe-symbol-aggregator.elc
	$(EMACS) -Q --batch -L . -l describe-symbol-aggregator -f describe-symbol-aggregator

%.elc: %.el
	${EMACS} -Q --batch -f batch-byte-compile $<
