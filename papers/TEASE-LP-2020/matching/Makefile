OUT=matching
.PHONY: $(OUT).pdf clean celan

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

