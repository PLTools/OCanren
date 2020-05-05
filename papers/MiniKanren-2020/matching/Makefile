.PHONY: celan clean all 

MAIN=matching
.PHONY: $(MAIN).pdf clean

all: $(MAIN).pdf

OCANREN_TEX=$(MAIN).tex

$(MAIN).pdf: $(OCANREN_TEX)
	pdflatex $<
	bibtex $(MAIN).aux
	pdflatex $<
	pdflatex $<

celan: clean
clean:
	$(RM) -f *.synctex.gz *.bak *.out *.aux *.bbl *.blg *.log $(MAIN).pdf 

