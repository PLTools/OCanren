.PHONY: main.pdf 

all: main.pdf

OCANREN_TEX=*.tex

main.pdf: $(OCANREN_TEX)
	pdflatex main.tex
#	bibtex main
#	pdflatex main.tex
	pdflatex main.tex


clean:
	rm -rf *~ *.log *.bak *.aux
