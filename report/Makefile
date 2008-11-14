NAME=gf-reason

LHS_INCLUDES= \
  gf.fmt \
  semantics.fmt \
  semantics.tex.lhs \
  answers.tex.lhs \
  ../Sem.tex.lhs \
  examples/toy/Toy.gf.lhs \
  examples/toy/ToyEng.gf.lhs \
  examples/toy/SemToy.tex.lhs

LHS2TEX_FLAGS= --path=..:

GHCI_FLAGS=-fwarn-incomplete-patterns -fwarn-missing-signatures -i.. -iexamples/toy

.PHONY: default pdf clean showpdf

default: pdf

pdf: $(NAME).pdf

$(NAME).pdf: $(NAME).tex
	pdflatex $(NAME)
	bibtex $(NAME)
	pdflatex $(NAME)
	pdflatex $(NAME)
	pdflatex $(NAME)

semantics.pdf: semantics.tex
	pdflatex semantics
	bibtex semantics
	pdflatex semantics
	pdflatex semantics

answers.pdf: answers.tex
	pdflatex answers
	bibtex answers
	pdflatex answers
	pdflatex answers

showsem: semantics.pdf
	acroread $^

%.tex: %.tex.lhs $(LHS_INCLUDES)
	lhs2TeX $(LHS2TEX_FLAGS) -o $@ $<

%.gf.lhs: %.gf
	cat gf-lhs-preamble.lhs > $@
	grep -v '^--' $< >> $@
	cat gf-lhs-postamble.lhs >> $@

%.hs: %.tex.lhs
	lhs2TeX $(LHS2TEX_FLAGS) --newcode $< > $@

examples/toy/Toy.hs examples/toy/Toy.pgf: examples/toy/ToyEng.gf
	gfc --make --output-format=haskell --haskell-prefix='' -D examples/toy $^

examples/toy/SemToy1.hs: semantics.tex.lhs $(LHS_INCLUDES)
	lhs2TeX $(LHS2TEX_FLAGS) -ssem_toy_1_code --newcode $< > $@

testtoy1: examples/toy/SemToy1.hs examples/toy/Toy.hs
	ghci $(GHCI_FLAGS) $<

examples/toy/SemToy2.hs: semantics.tex.lhs $(LHS_INCLUDES)
	lhs2TeX $(LHS2TEX_FLAGS) -ssem_toy_2_code --newcode $< > $@

testtoy2: examples/toy/SemToy2.hs examples/toy/Toy.hs
	ghci $(GHCI_FLAGS) $<

examples/toy/SemToy3.hs: semantics.tex.lhs $(LHS_INCLUDES)
	lhs2TeX $(LHS2TEX_FLAGS) -ssem_toy_3_code --newcode $< > $@

testtoy3: examples/toy/SemToy3.hs examples/toy/Toy.hs
	ghci $(GHCI_FLAGS) $<

examples/toy/SemToy4.hs: semantics.tex.lhs $(LHS_INCLUDES)
	lhs2TeX $(LHS2TEX_FLAGS) -ssem_toy_4_code --newcode $< > $@

testtoy4: examples/toy/SemToy4.hs examples/toy/Toy.hs
	ghci $(GHCI_FLAGS) $<


clean:
	-rm -f $(NAME).tex
	-rm -f *.aux *.dvi *.log *.blg *.bbl *.toc *.rel *.ptb *.out
	-rm -f *.pdf *.ps

showpdf: $(NAME).pdf
	acroread $(NAME).pdf
