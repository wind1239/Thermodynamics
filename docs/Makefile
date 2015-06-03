#! /bin/bash


default: Integrated_Compositional.pdf


Integrated_Compositional.pdf: Integrated_Compositional.tex

%.pdf:	%.tex
	rm -f $*.pdf
	pdflatex -shell-escape $<
	pdflatex -shell-escape $<
	if fgrep "Rerun to" $*.log; then $(MAKE) --assume-new $< $@;fi
ifndef NOBIBTEX
	if fgrep "There were undefined"  $*.log;\
	then bibtex $*; \
	$(MAKE) NOBIBTEX=true --assume-new $< $@;fi
endif
#	pdflatex -shell-escape $<
#	pdflatex -shell-escape $<

clean:
	rm -rf *.aux *.log *.blg *.bbl *.spl *.log *.out *-eps-converted-to.pdf *lof *toc \
	Integrated_Compositional.pdf
