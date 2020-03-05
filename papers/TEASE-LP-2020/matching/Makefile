OUT=matching
.PHONY: $(OUT).pdf clean celan spell

all: $(OUT).pdf

FILES_TEX=$(OUT).tex

$(OUT).pdf: $(FILES_TEX)
	pdflatex $<
	bibtex $(OUT).aux
	pdflatex $<
	pdflatex $<

celan: clean
clean:
	$(RM) *.bak *.out *.pdf

spell:
	aspell --lang=en --mode=tex check $(OUT).tex

