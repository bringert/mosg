
GHCFLAGS = -package gf-embed -package folkung

.PHONY: mosg.cgi run ghci Union.gfcc GSyntax.hs clean distclean

mosg.cgi:
	ghc $(GHCFLAGS) --make -o $@ MainCGI.hs

mosg:
	ghc $(GHCFLAGS) --make -o $@ Main.hs

Union.gfcc GSyntax.hs:
	gfc --make -haskell grammar/UnionEng.gf grammar/UnionSwe.gf

install:
	mkdir -p $(INSTALL_DIR)
	chmod a+w $(INSTALL_DIR)
	cp mosg.cgi Union.gfcc $(INSTALL_DIR)

clean:
	-rm -f *.o *.hi grammar/*.gfc grammar/*.gfr grammar/*.gfo
	-rm -f mosg.cgi

distclean: clean
	-rm -f Restrict.gfcc GSyntax.hs