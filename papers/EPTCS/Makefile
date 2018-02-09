.PHONY: ocanren.pdf clean

all: ocanren.pdf

OCANREN_TEX=ocanren.tex evaluation.tex conclusion.tex demo.tex \
	examples.tex goals.tex injection.tex introduction.tex \
	polymorphic-unification.tex reification.tex

ocanren.pdf: $(OCANREN_TEX)
	pdflatex $<
	bibtex ocanren.aux
	pdflatex $<
	pdflatex $<

clean:
	$(RM) *.bak *.out

