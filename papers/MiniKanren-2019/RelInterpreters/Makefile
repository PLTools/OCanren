.PHONY: main.pdf clean

all: main.pdf

OCANREN_TEX=main.tex

main.pdf: $(OCANREN_TEX)
	pdflatex $<
	bibtex main.aux
	pdflatex $<
	pdflatex $<

clean:
	$(RM) *.bak *.out *.aux *.bbl *.blg *.log main.pdf 

