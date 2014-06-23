MAIN=5-fold-notes
DATA=DC-posbi-rfl.txt \
	DC-postri-rfl.txt \
	TJ-posbi-rfl.txt \
	TJ-wuni-rfl-5000.txt \
	DC-wuni-rfl.txt \
	TJ-postri-rfl-5000.txt \

#PLOTS=$(DATA:.txt=-psize-ri-1-10.tikz.tex)
PLOTS=$(DATA:.txt=-psize-ri-1-10.pdf)

TARGETS=$(DATA:.txt=.5fold.rda) $(PLOTS) $(MAIN).pdf
#LATEX=pdflatex -shell-escape -interaction=batchmode
LATEX=xelatex -shell-escape #-interaction=batchmode
SOURCES=$(MAIN).tex # more...
FIGURES=
SUBDIRS=
BIBSRC=/data/lib/cc.bib
BIBTEX=biber

R=R --no-save --no-restore -q 
DATADIR=../../data/processed-data

.PHONY: subdirs all

all: $(TARGETS)

subdirs: 
	for dir in $(SUBDIRS); do  $(MAKE) -j 2 -C $$dir;  done

$(MAIN).pdf: $(SOURCES) $(FIGURES) $(MAIN).bib
	$(LATEX) $(MAIN)
	$(BIBTEX) $(MAIN)
	test -f $(MAIN).makefile && (sed -i 's/\^\^I/\t/g' $(MAIN).makefile ; make -j 2 -f $(MAIN).makefile) || echo no $(MAIN).makefile
	$(LATEX) $(MAIN)
	$(LATEX) $(MAIN)

$(MAIN).aux:
	$(LATEX) $(MAIN)

$(MAIN).bib: $(BIBSRC) $(MAIN).aux
	bibtool -q -i $(BIBSRC) \
   		$$(grep '@cite{' $(MAIN).aux |sed 's/.*@cite{/ -X /;s/}.*//'|sort|uniq) \
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

%-psize-ri-1-10.tikz.tex: plot-rand-index.R %.5fold.rda 
	$(R) -f $< --args $(@:-psize-ri-1-10.tikz.tex=) || (rm -f $@; exit 1)

%-psize-ri-1-10.pdf: plot-rand-index.R %.5fold.rda 
	$(R) -f $< --args $(@:-psize-ri-1-10.pdf=) || (rm -f $@; exit 1)

%.5fold.rda: 5-fold-cv.R $(DATADIR)/%.txt
	time $(R) -f $< --args $(@:.5fold.rda=) $(DATADIR) || (rm -f $@; exit 1)
