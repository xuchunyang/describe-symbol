EMACS = emacs

.PHONY: json
json:
	$(EMACS) -Q --batch -l describe-symbol-aggregator.el -f describe-symbol-aggregator
