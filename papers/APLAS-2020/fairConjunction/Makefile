.PHONY: celan clean all 

MAIN=fair-conj
.PHONY: $(MAIN).pdf clean

all: $(MAIN).pdf

OCANREN_TEX=$(MAIN).tex

COMPILER=pdflatex -interaction=nonstopmode

$(MAIN).pdf: $(OCANREN_TEX)
	$(COMPILER) $<
	bibtex $(MAIN).aux
	$(COMPILER) $<
	$(COMPILER) $<

celan: clean
clean:
	$(RM) -f *.synctex.gz *.bak *.out *.aux *.bbl *.blg *.log $(MAIN).pdf 

