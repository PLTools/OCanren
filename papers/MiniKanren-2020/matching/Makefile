.PHONY: celan clean all might might-weasel might-passive might-dups

MAIN=matching
.PHONY: $(MAIN).pdf clean deps

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

deps:
	sudo apt install texlive-fonts-extra

might: might-weasel might-passive might-dups
might-weasel:	
	echo "weasel words: "
	sh ../../bin/weasel *.tex
	echo

might-passive:	
	echo "passive voice: "
	sh ../../bin/passive *.tex
	echo

might-dups:	
	echo "duplicates: "
	perl ../../bin/dups *.tex
	echo
