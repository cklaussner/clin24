
#source("log-smoothing.R")


# function to compute relative Frequency weighting + Laplace smoothing (alpha = 1) and taking logs after
# input document-term matrix - orientation important

relFreq.weight <-function(nV){
  Vsize <- dim(nV)
  numOfDocs <- Vsize[1]
  numOfTerms <- Vsize[2]
  
  n.relFreq <- matrix(0,nrow=numOfDocs,ncol= numOfTerms)
  rownames(n.relFreq) <- rownames(nV)
  colnames(n.relFreq) <- colnames(nV)
  for (n in rownames(nV)){
    
    curr.Doc <- nV[n,]
    w.Token <- sum(curr.Doc) # sum over all tokens in doc
    w.Type <- length(curr.Doc[curr.Doc != 0])
    
    for(i in 1:length(curr.Doc)){
      
      curr.Val <- curr.Doc[i]
      
      rel.Freq <- (curr.Val +1)/ (w.Token+w.Type)
      
      n.relFreq[n,i] <- log(rel.Freq)
    }
    
  }
  
  return(n.relFreq)
}
