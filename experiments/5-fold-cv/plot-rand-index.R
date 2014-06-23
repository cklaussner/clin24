#
# This script creates plots of avarage rand index over multiple folds.
# The input file is one of the Xfold.rda files in this directory.
# The script uses calc.cluster.metrics() from cluster-eval.R in this 
# directory to get the rand index for each fold.
#

args <- commandArgs(trailingOnly = TRUE)
basename <- as.character(args[1])
infile <- paste(basename, '.5fold.rda', sep='')
psize.start <- ifelse(is.na(args[2]), 1, args[2])
psize.incr  <- ifelse(is.na(args[3]), 10, args[3])

                 
width   <- 3.5  # approximately double-column \linewith on A4 paper
height  <- 3.5  #

require(mclust)
source('cluster-eval.R')

outfile <- paste(basename, "-psize-ri-", 
                 psize.start, "-", psize.incr, 
#                 '.tikz.tex',
                 '.pdf',
                 sep="")
#require(tikzDevice)
#tikz(outfile, width=height, height=height)
pdf(outfile, width=height, height=height)

cat("Loading:", infile, "\n")
data.names <- load(infile)
data <- get(data.names[grep('^data', data.names)])
rd <- get(data.names[grep('^rd', data.names)])

nterms <- length(colnames(data))

psize.range <- seq(psize.incr, nterms, by=psize.incr)
mean.ri <- list(zr.set1=c(),
                zr.set2=c(),
                zd=c(),
                zrd.set1=c(),
                zrd.set2=c()
           )

for(psize in psize.range) {
    for (val in names(mean.ri)){
        cdata <- calc.cluster.metrics(data, rd, 
                                      start=1,
                                      end=psize-1,
                                      feature=val)
        mean.ri[[val]] <- c(mean.ri[[val]], mean(cdata$ri))
    }
}

plot(0, type='n', 
     ylim=c(0,1), xlim=range(psize.range),
     xlab='Profile size',
     ylab='Adjusted Rand Index (average of 5 folds)',
     main=basename)

for (i in 1:length(names(mean.ri))){
    print(summary(mean.ri[[i]]))
    lines(mean.ri[[i]] ~ psize.range, col=i)
}

legend('topright', names(mean.ri), lty=1, col=1:length(names(mean.ri)), bty='n')

dev.off()
