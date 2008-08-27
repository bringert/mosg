
NAME = Sem

GHCFLAGS = -package gf -package folkung

INSTALL_DIR = $(HOME)/public_html/mosg

.PHONY: mosg.cgi mosg.fcgi mosg mosg-fracas test Syntax.pgf Syntax.hs showpdf install clean distclean

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
	lhs2TeX --newcode -sunhandled Sem.tex.lhs > Sem.hs

unhandled:
	lhs2TeX --newcode Sem.tex.lhs > unhandled.hs
	echo | ghci unhandled.hs

%.tex: %.tex.lhs
	lhs2TeX $^ > $@

Sem.pdf: Sem.tex
	pdflatex Sem.tex
	bibtex Sem
	pdflatex Sem.tex
	pdflatex Sem.tex

showpdf: Sem.pdf
	acroread $^

InterExample.hs: InterExample.tex.lhs
	lhs2TeX --newcode $^ > $@

InterExample.pdf: InterExample.tex
	pdflatex InterExample.tex

Syntax.pgf Syntax.hs:
	gfc --make --output-format=haskell --name=Syntax grammar/English.gf # grammar/Swedish.gf # grammar/Norwegian.gf grammar/German.gf

install:
	mkdir -p $(INSTALL_DIR)
	chmod a+w $(INSTALL_DIR)
	cp mosg.cgi Syntax.pgf $(INSTALL_DIR)

clean:
	-rm -f *.aux *.dvi *.log *.blg *.bbl *.toc *.rel *.ptb
	-rm -f $(NAME).pdf
	-rm -f *.o *.hi grammar/*.gfc grammar/*.gfr grammar/*.gfo
	-rm -f mosg.cgi mosg.fcgi mosg mosg-fracas

distclean: clean
	-rm -f Syntax.pgf Syntax.hs