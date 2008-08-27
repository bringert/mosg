NAME=gf-reason

LHS_INCLUDES= \
  ../Sem.tex.lhs

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

clean:
	-rm -f $(NAME).tex
	-rm -f *.aux *.dvi *.log *.blg *.bbl *.toc *.rel *.ptb *.out
	-rm -f *.pdf *.ps

showpdf: $(NAME).pdf
	acroread $(NAME).pdf
