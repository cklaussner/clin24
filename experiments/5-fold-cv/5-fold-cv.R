
args <- commandArgs(trailingOnly = TRUE)
basename <- as.character(args[1])
datadir <- as.character(args[2])
infile <- paste(datadir, '/', basename, '.txt', sep='')
outfile <- paste(basename, '.5fold.rda', sep='')
set1author <- substr(basename, 1, 1)

source("../../code/eval/n-fold-rd.R")

data <- as.matrix(read.table(infile))

if (grepl('^DC', basename)) {
    # exclude multi-author work 
    data <- data[grep('^[DC][0-9]', rownames(data)),]
}

rd.name <- paste('rd', gsub('-', '.', basename), sep='.')
assign(rd.name,
       n.fold.RD(data,
                 substr(rownames(data), 1, 1) == set1author,
                 substr(rownames(data), 1, 1) != set1author)
)

data.name <- paste('data', gsub('-', '.', basename), sep='.')
assign(data.name, data)

save(list=c(data.name, rd.name), file=outfile)
