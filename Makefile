GHCFLAGS=-Wall -fno-warn-name-shadowing -XHaskell98 -O2
HLINTFLAGS=-XHaskell98 -XCPP -i 'Use camelCase' -i 'Use String' -i 'Use head' -i 'Use string literal' -i 'Use list comprehension' --utf8
VERSION=0.2

.PHONY: all shell clean doc install

all: report.html doc dist/build/libHSwai-session-$(VERSION).a dist/wai-session-$(VERSION).tar.gz

install: dist/build/libHSwai-session-$(VERSION).a
	cabal install

shell:
	ghci $(GHCFLAGS)

report.html: Network/Wai/Session.hs Network/Wai/Session/Map.hs
	-hlint $(HLINTFLAGS) --report $^

doc: dist/doc/html/wai-session/index.html README

README: wai-session.cabal
	tail -n+$$(( `grep -n ^description: $^ | head -n1 | cut -d: -f1` + 1 )) $^ > .$@
	head -n+$$(( `grep -n ^$$ .$@ | head -n1 | cut -d: -f1` - 1 )) .$@ > $@
	-printf ',s/        //g\n,s/^.$$//g\n,s/\\\\\\//\\//g\nw\nq\n' | ed $@
	$(RM) .$@

dist/doc/html/wai-session/index.html: dist/setup-config Network/Wai/Session.hs Network/Wai/Session/Map.hs
	cabal haddock --hyperlink-source

dist/setup-config: wai-session.cabal
	cabal configure

clean:
	find -name '*.o' -o -name '*.hi' | xargs $(RM)
	$(RM) -r dist

dist/build/libHSwai-session-$(VERSION).a: dist/setup-config Network/Wai/Session.hs Network/Wai/Session/Map.hs
	cabal build --ghc-options="$(GHCFLAGS)"

dist/wai-session-$(VERSION).tar.gz: README dist/setup-config Network/Wai/Session.hs Network/Wai/Session/Map.hs
	cabal check
	cabal sdist
