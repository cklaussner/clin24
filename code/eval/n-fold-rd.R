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

#     docdif <- list()    # this stores doc-doc differences per term 
#                         # (or feature for speed
# 
#     for (term in colnames(data)) {
#         docdif[[term]] <- matrix(NA, 
#                                nrow = length(alldocs), 
#                                ncol = length(alldocs),
#                                dimnames = list(alldocs, alldocs))
#         for (doc1 in c(set1docs, set2docs)) {
#             dif.sum <- 0
#             for (doc2 in c(set1docs, set2docs)) {
#                 dif <- abs(data[doc1, term] - data[doc2, term])
#                 dif.sum = dif.sum + dif
#                 docdif[[term]][doc1, doc2] <- dif.sum
#             }
#         }
#     }

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

        rd[[fold.name]]$r <- list(set1=c(), set2=c())
        rd[[fold.name]]$d <- c()
        rd[[fold.name]]$zr <- list(set1=c(), set2=c())
        rd[[fold.name]]$zd <- c()
        for (term in colnames(data)) {
            # tmp <- rd.sum(set1[set1.trn,term],set2[set2.trn,term])
            # rd[[fold.name]]$r$set1[term] <- norm.r.set1 * tmp[1]
            # rd[[fold.name]]$r$set2[term] <- norm.r.set2 * tmp[2]
            # rd[[fold.name]]$d[term] <- norm.d * tmp[3]
            set1.dif <- within.diff(set1[set1.trn,term])
            set2.dif <- within.diff(set2[set2.trn,term])
            set12.dif <- between.diff(set1[set1.trn,term],set2[set2.trn,term])
            rd[[fold.name]]$r$set1[term] <- norm.r.set1 * sum(set1.dif)
            rd[[fold.name]]$r$set2[term] <- norm.r.set2 * sum(set2.dif)
            rd[[fold.name]]$d[term] <- norm.d * sum(set12.dif)

            m <- mean(c(set1.dif, set2.dif, set12.dif))
            s <- sd(c(set1.dif, set2.dif, set12.dif))

            rd[[fold.name]]$zr$set1[term] <- (rd[[fold.name]]$r$set1[term] - m) / s
            rd[[fold.name]]$zr$set2[term] <- (rd[[fold.name]]$r$set2[term] - m) / s
            rd[[fold.name]]$zd[term] <- (rd[[fold.name]]$d[term] - m) / s
        }
    }


    return(rd)
}

#
# This is a re-parametrized version of the represent() function.  The
# single argument is the a vector of values (frequencies) for a single
# feature (term). The return value is the sum of the absolute differences
# within the document set.
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
# single feature (term). The return value is the sum of the absolute
# differences between the two document sets.
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


#
# This is version is a dynamic programming implementation
# it should speed up the calculations substantially, but currently it is broken.
#
# repr.sum <- function(part1, part2, docdif) {
#     rsum <- 0
#     for (doc in c(part1, part2)) {
#         dif.part1 <- ifelse(is.null(part1),
#                             0,
#                             docdif[doc, part1[length(part1)]]
#         )
#         cat("---dif:", " (", doc, ")", dif.part1, docdif[doc, part1[length(part1)]], "-", docdif[doc, part1[1]])
#         dif.part2 <- ifelse(is.null(part2),
#                             0, 
#                             docdif[doc, part2[length(part2)]] - 
#                             docdif[doc, part2[1]]
#         )
#         cat(" +", dif.part2, docdif[doc, part2[length(part2)]], "-", docdif[doc, part2[1]], "\n")
#         rsum <- rsum + dif.part1 + dif.part2
#     }
#     print(docdif)
#     return(rsum)
# }
