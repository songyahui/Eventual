MAIN = main
LATEX = pdflatex -synctex=1
BIBTEX = bibtex

.PHONY: all clean

all: $(MAIN).pdf

$(MAIN).pdf: $(MAIN).tex references.bib
	$(LATEX) $(MAIN)
	$(BIBTEX) $(MAIN)
	$(LATEX) $(MAIN)
	$(LATEX) $(MAIN)

clean:
	rm -f *.pdf *.aux *.bbl *.blg *.log *.out *.toc *.fls *.fdb_latexmk \
	      *.synctex.gz *.bcf *.run.xml
