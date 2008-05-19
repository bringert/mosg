
GHCFLAGS = -package gf-embed -package folkung

INSTALL_DIR = $(HOME)/public_html/mosg

.PHONY: mosg.cgi mosg.fcgi mosg mosg-fracas test Union.gfcc GSyntax.hs showpdf install clean distclean

mosg.fcgi: Sem.hs
	ghc $(GHCFLAGS) -package fastcgi --make -o $@ MainFastCGI.hs

mosg.cgi: Sem.hs
	ghc $(GHCFLAGS) --make -o $@ MainCGI.hs

mosg: Sem.hs
	ghc $(GHCFLAGS) --make -o $@ Main.hs

mosg-fracas: Sem.hs
	ghc $(GHCFLAGS) -package HaXml-1.13.3 --make -o $@ MainFraCaS.hs

test:
	ghc $(GHCFLAGS) -i../embedded-gf/src --make -o $@ test.hs

Sem.hs: Sem.tex.lhs
	lhs2tex --newcode -sunhandled Sem.tex.lhs > Sem.hs

unhandled:
	lhs2tex --newcode Sem.tex.lhs > unhandled.hs
	echo | ghci unhandled.hs

Sem.pdf: Sem.tex.lhs
	lhs2tex Sem.tex.lhs > Sem.tex
	pdflatex Sem.tex
	pdflatex Sem.tex
	pdflatex Sem.tex

showpdf: Sem.pdf
	acroread $^

Union.gfcc GSyntax.hs:
	gfc --make -haskell grammar/UnionEng.gf # grammar/UnionSwe.gf grammar/UnionNor.gf grammar/UnionGer.gf

install:
	mkdir -p $(INSTALL_DIR)
	chmod a+w $(INSTALL_DIR)
	cp mosg.cgi Union.gfcc $(INSTALL_DIR)

clean:
	-rm -f *.o *.hi grammar/*.gfc grammar/*.gfr grammar/*.gfo
	-rm -f mosg.cgi mosg.fcgi mosg mosg-fracas

distclean: clean
	-rm -f Union.gfcc GSyntax.hs