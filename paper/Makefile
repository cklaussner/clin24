MAIN=clin
TARGETS=$(MAIN).pdf #and more..
LATEX=pdflatex -shell-escape -interaction=batchmode
#LATEX=xelatex -shell-escape #-interaction=batchmode
SOURCES=$(MAIN).tex # more...
FIGURES=
SUBDIRS=
BIBSRC=/data/lib/cc.bib
BIBTEX=bibtex

.PHONY: subdirs all

all: $(MAIN).pdf

subdirs: 
	for dir in $(SUBDIRS); do  $(MAKE) -j 2 -C $$dir;  done

$(MAIN).pdf: $(SOURCES) $(FIGURES) $(MAIN).bib
	$(LATEX) $(MAIN)
	$(BIBTEX) $(MAIN)
	test -f $(MAIN).makefile && make -j 2 -f $(MAIN).makefile || echo no $(MAIN).makefile
	$(LATEX) $(MAIN)
	$(LATEX) $(MAIN)

$(MAIN).aux:
	$(LATEX) $(MAIN)

$(MAIN).bib: $(BIBSRC) $(MAIN).aux
	bibtool -q -i $(BIBSRC) \
   		$$(grep 'cite{' $(MAIN).aux |sed 's/.*cite{/ -X /;s/}.*//'|sort|uniq) \
       	> $(MAIN).bib

view:
	okular $(MAIN).pdf >/dev/null 2>&1 &

clean:
	rm -f *~ $(MAIN).aux $(MAIN).bbl $(MAIN).blg $(MAIN).log\
		$(MAIN).pdf $(MAIN).snm $(MAIN).toc $(MAIN).nav $(MAIN).out \
	 	$(MAIN).bcf   missfont.log $(MAIN).run.xml  

dist-clean:
	make clean
	rm -f $(MAIN).bib
