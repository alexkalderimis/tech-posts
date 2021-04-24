SITE = .stack-work/build/site/site
SRC = $(shell find src -name "*.hs")

$(SITE): alex-kalderimis-dev.cabal $(SRC)
	stack build

_site: $(SITE) $(SRC)
	stack exec site rebuild

.PHONY: watch
.watch:
	stack run site -- watch

.PHONY: clean
clean:
	rm -rf _site
	rm -rf _cache
	rm -rf .stack-work

