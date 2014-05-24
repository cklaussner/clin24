library(tm)

# source("unigrams.R")


# to compute document-term matrixes with simple term-frequency weighting, reduce sparsity from directory input

createDTM <- function(location){
  
  gsub2<- function(myPattern, myReplacement, myCorpus, fixed=FALSE,ignore.case=FALSE){
    for (i in 1:length(myCorpus)){
      for (j in 1:length(myPattern)){
        myCorpus[[i]]<- gsub(myPattern[j],myReplacement[j], myCorpus[[i]], fixed=TRUE)
      }
    }
    return(myCorpus)
  }
  
  
  docCorpus <- Corpus(DirSource(location),readerControl = list(language = "en"))   
  docCorpus <- gsub2("-"," ",docCorpus,fixed=TRUE,ignore.case=TRUE)
  
  docCorpus <- tm_map(docCorpus,removePunctuation)
  docCorpus <- tm_map(docCorpus,removeNumbers)
  docCorpus  <- tm_map(docCorpus,tolower)
  dtm <- DocumentTermMatrix(docCorpus)
  dtm <- as.matrix(removeSparseTerms(dtm, 0.7))
  dtm <- dtm[,order(colSums(dtm),decreasing = TRUE)]
  
  
return(dtm)
  
}
