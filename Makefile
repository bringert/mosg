
GF_PATH = $(HOME)/Projects/GF/src

GHCFLAGS = -i$(GF_PATH)  

.PHONY: mosg.cgi run ghci Union.gfcc GSyntax.hs clean distclean

mosg.cgi:
	ghc $(GHCFLAGS) --make -o $@ MainCGI.hs

run:
	runghc $(GHCFLAGS) Main.hs

ghci:
	ghci $(GHCFLAGS) Main.hs

Union.gfcc GSyntax.hs:
	gfc --make -haskell grammar/UnionEng.gf grammar/UnionGer.gf

clean:
	-rm -f *.o *.hi grammar/*.gfc grammar/*.gfr grammar/*.gfo
	-rm -f mosg.cgi

distclean: clean
	-rm -f Restrict.gfcc GSyntax.hs