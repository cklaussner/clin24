#
n.fold.RD <- function(data, 
                      set1index, 
                      set2index, 
                      folds=5, 
                      randomize=F)
{
    set1 <- data[set1index,]
    set2 <- data[set2index,]
    set1docs <- row.names(set1)
    set2docs <- row.names(set2)
    alldocs <- c(set1docs, set2docs)
    nSet1 <- length(set1docs)
    nSet2 <- length(set2docs)
    foldsizeS1 <- nSet1 / folds
    foldsizeS2 <- nSet2 / folds

    rd <- list()        # we will save and return the results here

    for (fold in 1:folds) {
        fold.name <- paste('fold',fold,sep="")
        rd[[fold.name]] <- list()

        # calculate the (numeric) indexes corresponding to the test
        # documents in each set
        set1.tst.start <- as.integer((fold - 1) * foldsizeS1) + 1
        set2.tst.start <- as.integer((fold - 1) * foldsizeS2) + 1
        if (fold == folds) { # use all remaining documents for the last fold
            set1.tst.end <- nSet1
            set2.tst.end <- nSet2
        } else {
            set1.tst.end <- as.integer(set1.tst.start + foldsizeS1) - 1
            set2.tst.end <- as.integer(set2.tst.start + foldsizeS2) - 1
        }
        set1.tst <- set1.tst.start:set1.tst.end
        set2.tst <- set2.tst.start:set2.tst.end

        # calculate the (numeric) indexes corresponding to the
        # training documents in each set
        if (fold == 1) {
            set1.trn.part1 <- NULL 
            set2.trn.part1 <- NULL
            set1.docs.part1 <- NULL
            set2.docs.part1 <- NULL
        } else {
            set1.trn.part1 <- 1:(set1.tst.start - 1)
            set2.trn.part1 <- 1:(set2.tst.start - 1)
            set1.docs.part1 <- set1docs[set1.trn.part1]
            set2.docs.part1 <- set2docs[set2.trn.part1]
        }


        if (fold == folds) {
            set1.trn.part2 <- NULL 
            set2.trn.part2 <- NULL
            set1.docs.part2 <- NULL
            set2.docs.part2 <- NULL
        } else {
            set1.trn.part2 <- (set1.tst.end+1):nSet1
            set2.trn.part2 <- (set2.tst.end+1):nSet2
            set1.docs.part2 <- set1docs[set1.trn.part2]
            set2.docs.part2 <- set2docs[set2.trn.part2]
        }

        set1.trn <- c(set1.trn.part1, set1.trn.part2)
        set2.trn <- c(set2.trn.part1, set2.trn.part2)

        # record the names of training and test docuemnts
        rd[[fold.name]]$testdocs.set1 <- set1docs[set1.tst]
        rd[[fold.name]]$testdocs.set2 <- set2docs[set2.tst]
        rd[[fold.name]]$traindocs.set1 <- set1docs[set1.trn]
        rd[[fold.name]]$traindocs.set2 <- set2docs[set2.trn]

        set1.trn.n <- length(set1.trn)
        set2.trn.n <- length(set2.trn)
        norm.r.set1 <- 2 / (set1.trn.n^2 - set1.trn.n)
        norm.r.set2 <- 2 / (set2.trn.n^2 - set2.trn.n)

        norm.d <- 1 / (set1.trn.n * set2.trn.n)

        rd[[fold.name]]$r.set1 <- c()   # representativeness for set1
        rd[[fold.name]]$r.set2 <- c()   # and set 2
        rd[[fold.name]]$d <- c()        # distinctiveness
        rd[[fold.name]]$zr.set1 <- c()  # z-score for r.set1
        rd[[fold.name]]$zr.set2 <- c()  # z-score for r.set2
        rd[[fold.name]]$zd <- c()       # z-score for distinctiveness
        # the following two are just difference `d - r', we calculate
        # here for conveninece
        rd[[fold.name]]$zrd.set1 <- c() # combined r&d for set1 (z-score only)
        rd[[fold.name]]$zrd.set2 <- c() # combined r&d for set2 (z-score only)
        for (term in colnames(data)) {
            set1.dif <- within.diff(set1[set1.trn,term])
            set2.dif <- within.diff(set2[set2.trn,term])
            set12.dif <- between.diff(set1[set1.trn,term],set2[set2.trn,term])
            rd[[fold.name]]$r.set1[term] <- norm.r.set1 * sum(set1.dif)
            rd[[fold.name]]$r.set2[term] <- norm.r.set2 * sum(set2.dif)
            rd[[fold.name]]$d[term] <- norm.d * sum(set12.dif)

            m <- mean(c(set1.dif, set2.dif, set12.dif))
            s <- sd(c(set1.dif, set2.dif, set12.dif))

            rd[[fold.name]]$zr.set1[term] <- (rd[[fold.name]]$r.set1[term] - m) / s
            rd[[fold.name]]$zr.set2[term] <- (rd[[fold.name]]$r.set2[term] - m) / s
            rd[[fold.name]]$zd[term] <- (rd[[fold.name]]$d[term] - m) / s

            rd[[fold.name]]$zrd.set1[term] <- rd[[fold.name]]$zd[term] - rd[[fold.name]]$zr.set1[term] 
            rd[[fold.name]]$zrd.set2[term] <- rd[[fold.name]]$zd[term] - rd[[fold.name]]$zr.set2[term] 
        }
    }


    return(rd)
}

#
# This is a re-parametrized version of the represent() function.  The
# single argument is a vector of values (frequencies) for a single
# feature (term). The return value is the absolute differences
# within the document set (as a vector).
#
within.diff <- function(docset) {
    ret <- c()
    n <- length(docset)
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n){
            ret <- c(ret, abs(docset[i] - docset[j]))
        }
    }
    return(ret)  
}
 
#
# This is a re-parametrized version of the distinct() function.  The
# single arguments are two vectors of values (frequencies) for a
# single feature (term). The return value is the absolute
# differences between the two document sets (as a vector).
#
between.diff <- function(docs1, docs2) {
    ret <- c()
    n1 <- length(docs1)
    n2 <- length(docs2)

    for (i in 1:n1){
        for(j in 1:n2){
            ret <- c(ret, abs(docs1[i] - docs2[j]))
        }
    }
    return(ret)
}

#
#
rd.sum <- function(docs1, docs2) {
    rsum.set1 <- 0
    rsum.set2 <- 0
    dsum <- 0

    n1 <- length(docs1)
    n2 <- length(docs2)
    N <- n1 + n2

    for (i in 1:(N - 1)){
        for(j in (i+1):N){
            if (i <= n1) {
                if (j <= n1) { # both in set1
                    rsum.set1 <- rsum.set1 + abs(docs1[i] - docs1[j])
                } else { # i in set1 j in set2
                    dsum <- dsum + abs(docs1[i] - docs2[j - n1])
                }
            } else {
                if (j > n1) { # both in set2
                    rsum.set2 <- rsum.set2 + abs(docs2[i - n1] - docs2[j - n1])
                } else { # i in set2 j in set1
                    dsum <- dsum + abs(docs2[i - n1] - docs1[j])
                }
            }
        }
    }
    return(c(rsum.set1, rsum.set2, dsum))
}
