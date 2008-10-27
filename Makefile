
NAME = Sem

GHCFLAGS = 

INSTALL_DIR = $(HOME)/public_html/mosg

.PHONY: mosg.cgi mosg.fcgi mosg mosg-fracas semantics.fcgi reasoning.fcgi pgf.fcgi gwt test Syntax.pgf Syntax.hs showpdf install clean distclean

mosg.fcgi: Sem.hs
	ghc $(GHCFLAGS) -threaded -package gf -package folkung -package fastcgi --make -o $@ MainFastCGI.hs

mosg.cgi: Sem.hs
	ghc $(GHCFLAGS) -package gf -package folkung --make -o $@ MainCGI.hs

mosg: Sem.hs
	ghc $(GHCFLAGS) -package gf -package folkung --make -o $@ Main.hs

mosg-fracas: Sem.hs
	ghc $(GHCFLAGS) -package gf -package folkung -package HaXml-1.13.3 --make -o $@ MainFraCaS.hs

semantics.fcgi:
	ghc $(GHCFLAGS) -threaded -package gf -package fastcgi --make -o $@ SemanticsService.hs

reasoning.fcgi:
	ghc $(GHCFLAGS) -threaded -package folkung -package fastcgi --make -o $@ ReasoningService.hs

pgf.fcgi:
	cp ../gf/src/server/pgf.fcgi .

gwt:
	gwt/Mosg-compile

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
	gfc --make --output-format=haskell --haskell=lexical --lexical=N,N2,N3,PN,A,A2,V,V2,V3,Prep --name=Syntax grammar/English.gf # grammar/Swedish.gf # grammar/Norwegian.gf grammar/German.gf

run: pgf.fcgi semantics.fcgi reasoning.fcgi
	@echo '*********************************************'
	@echo 'See http://localhost:1970/'
	@echo '*********************************************'
	lighttpd -f lighttpd.conf -D

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