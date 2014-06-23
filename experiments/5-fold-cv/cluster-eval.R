calc.cluster.metrics <- function(data, rd, start=1, end=10, feature='zd') { 
    #
    # This function clusters the test documents listed in 'rd' using
    # the raw 'data', for each fold in 'rd'.  The clustering results
    # as well as a simple vector of 'Adjusted Rand index' for each
    # fold is returned.
    #

    adjri <- c()
    cllist <- list()
    for(fold in names(rd)){
        testdocs <- c(rd[[fold]]$testdocs.set1,rd[[fold]]$testdocs.set2) 
        gs <- c(rep(1,length(rd[[fold]]$testdocs.set1)),
                rep(2,1,length(rd[[fold]]$testdocs.set2)))
        names(gs) <- testdocs
        if(grepl('^z*r[.]', feature)){ 
            term.order <- order(rd[[fold]][[feature]], decreasing=F)
        } else {
            term.order <- order(rd[[fold]][[feature]], decreasing=T)
        }
        fold.data <- data[testdocs, names(rd[[fold]][[feature]])[term.order]]
        d <- dist(fold.data[,start:end])
        cl <- hclust(d)
        cllist[[fold]] <- c
        clcut <- cutree(cl, 2)
        adjri <- c(adjri, adjustedRandIndex(clcut, gs))
    }
    return(list(cl=cllist,ri=adjri))
}
