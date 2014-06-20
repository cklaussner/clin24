DATA=DC-posbi-rfl.txt \
	DC-postri-rfl.txt \
	TJ-posbi-rfl.txt \
	TJ-wuni-rfl-5000.txt \
	DC-wuni-rfl.txt \
	TJ-postri-rfl-5000.txt \

R=R --no-save --no-restore -q 
DATADIR=../../data/processed-data


TARGETS=$(DATA:.txt=.5fold.rda)

%.5fold.rda: 5-fold-cv.R $(DATADIR)/%.txt
	time $(R) -f $< --args $(@:.5fold.rda=) $(DATADIR) || (rm -f $@; exit 1)


all: $(TARGETS)
