
library(koRpus)
require("NLP")
library(openNLP)
library(stylo)
library(tm)

# source("pos-unigrams.R")

# script to compute POS bigrams/unigrams


#R CMD javareconf -e # run before starting R 

set.kRp.env(TT.cmd="/home/carmen/tcd/CarmenDropFiles/clin24/code/preprocessing/tagger/cmd/tree-tagger-english", lang="en") # hard-coded for English



computePOSbi <- function(pathToDir){
  tag.names <- kRp.POS.tags(lang = get.kRp.env(lang = TRUE),
                            list.classes = FALSE, list.tags = FALSE,
                            tags = c("words"))[,1]
  
  
  files <- list.files(pathToDir)
  labels.comb <- c()
  POSbi.col <- list()
  
  
  for (file in files){ 
    results <- list()
    print(file)
    fileDir <- paste(pathToDir,file,sep="")
    
    
    
    tagged.text <- treetag(fileDir, treetagger = "kRp.env",rm.sgml = TRUE,
                           lang = "kRp.env",encoding = NULL,TT.tknz = TRUE)
    
    
    # "sentc","punct"
    TT.tag <- tagged.text@TT.res[,"tag"] # put frequencies in matrix
    allowed.tags <- intersect(tag.names,TT.tag) # take only those that are legal ;-)
    tags.filt <- c()
    for (i in TT.tag){ if (i %in% allowed.tags){
      tags.filt <- c(tags.filt,i)
      
    }}
    
    posbi.list <- processPOSbi(tags.filt)
    POSbi.col[[file]] <- posbi.list
    labels.comb <- union(labels.comb, rownames(posbi.list))
    
  }
  
  posbi.df <- matrix(0,nrow=length(POSbi.col),ncol=length(labels.comb))
  colnames(posbi.df) <- labels.comb
  rownames(posbi.df) <- names(POSbi.col)
  
  for (n in names(POSbi.col)){
    
    curr.labels <- rownames(as.matrix(as.data.frame(POSbi.col[n])))
    posbi.df[n,curr.labels] <- as.matrix(as.data.frame(POSbi.col[n]))
    
  }
  
  return(posbi.df)
  
}

processPOStri <- function(tagged.text){
  
  postri.freq <- as.matrix(table(make.ngrams(tagged.text,ngram.size=3)))
  
  return(postri.freq)
}


processPOSbi <- function(tagged.text){
  
  posbi.freq <- as.matrix(table(make.ngrams(tagged.text,ngram.size=3)))
  
  return(posbi.freq)
}


