.SUFFIXES: .tex .pdf
.PHONY: matching-talk.pdf

all: matching-talk.pdf


define WRAP
INPUT_$(1)=$(wildcard $(1)*.tex)
.PHONY: $(1).pdf

$(1).pdf: $$(INPUT_$(1):%.tex=%.pdf)
$$(INPUT_$(1):%.tex=%.pdf): $$(INPUT_$(1)) heading.tex
	xelatex -shell-escape -interaction=nonstopmode $$<
	bibtex $(1)
endef

LECTURES=matching-talk

$(foreach i,$(LECTURES),$(eval $(call WRAP,$(i)) ) )

clean:
	@$(RM) -r _minted* 
	@$(RM) *.aux *.log *.nav *.out *.snm *.toc *.vrb *.synctex.gz *.bbl *.blg
	$(RM) *.pdf

