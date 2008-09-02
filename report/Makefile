NAME=gf-reason

LHS_INCLUDES= \
  ../Sem.tex.lhs \
  examples/toy/Toy.gf.lhs \
  examples/toy/ToyEng.gf.lhs \
  examples/toy/SemToy.tex.lhs

LHS2TEX_FLAGS= --path=..:

.PHONY: default pdf clean showpdf

default: pdf

pdf: $(NAME).pdf

$(NAME).pdf: $(NAME).tex
	pdflatex $(NAME)
	bibtex $(NAME)
	pdflatex $(NAME)
	pdflatex $(NAME)
	pdflatex $(NAME)

$(NAME).tex: $(LHS_INCLUDES)

%.tex: %.tex.lhs
	lhs2TeX $(LHS2TEX_FLAGS) -o $@ $<

%.gf.lhs: %.gf
	cat gf-lhs-preamble.lhs > $@
	grep -v '^--' $< >> $@
	cat gf-lhs-postamble.lhs >> $@

%.hs: %.tex.lhs
	lhs2TeX $(LHS2TEX_FLAGS) --newcode $< > $@

examples/toy/Toy.hs examples/toy/Toy.pgf: examples/toy/ToyEng.gf
	gfc --make --output-format=haskell -D examples/toy $^

examples/toy/SemToy1.hs: gf-reason.tex.lhs examples/toy/Toy.hs
	lhs2TeX $(LHS2TEX_FLAGS) -ssem_toy_1_code --newcode $< > $@

testtoy1: examples/toy/SemToy1.hs
	ghci -i.. -iexamples/toy $<

examples/toy/SemToy2.hs: gf-reason.tex.lhs examples/toy/Toy.hs
	lhs2TeX $(LHS2TEX_FLAGS) -ssem_toy_2_code --newcode $< > $@

testtoy2: examples/toy/SemToy2.hs
	ghci -i.. -iexamples/toy $<

examples/toy/SemToy3.hs: gf-reason.tex.lhs examples/toy/Toy.hs
	lhs2TeX $(LHS2TEX_FLAGS) -ssem_toy_3_code --newcode $< > $@

testtoy3: examples/toy/SemToy3.hs
	ghci -i.. -iexamples/toy $<

examples/toy/SemToy4.hs: gf-reason.tex.lhs examples/toy/Toy.hs
	lhs2TeX $(LHS2TEX_FLAGS) -ssem_toy_4_code --newcode $< > $@

testtoy4: examples/toy/SemToy4.hs
	ghci -i.. -iexamples/toy $<


clean:
	-rm -f $(NAME).tex
	-rm -f *.aux *.dvi *.log *.blg *.bbl *.toc *.rel *.ptb *.out
	-rm -f *.pdf *.ps

showpdf: $(NAME).pdf
	acroread $(NAME).pdf
