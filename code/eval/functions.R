library(mclust)
library(stats)
library(cluster)
#library(fpc)

# Functions more independent of application


check.data <- function(dataset,noOfD,noOfO){

if ((substr(rownames(dataset)[1],1,1) == "D")){
  print("Dickens is first")
  prim.set <- dataset[1:noOfD, ]
  sec.set <- dataset[(noOfD+1):(noOfD+noOfO), ]
}else{
  print("Comparison set is first")
  sec.set <- dataset[1:noOfO, ]
  prim.set <- dataset[(noOfO+1):(noOfD+noOfO), ]
}

return(rbind(prim.set, sec.set)) # have dataset in right order: Dickens-nonDickens
}

# computes combination of various dissim. Matrices for a list of features 
dissim.Matrix <- function(input,RD.features){

  m <- as.matrix(input[,RD.features])
  RD.matrix <- as.matrix((dist(m, method="manhattan", diag=TRUE, upper=TRUE)))# account for no of features compared

return(RD.matrix)

}

# compute corrected Rand ind. for clustering based on dissim. matrix 


getAdjRand <- function(dM, Metric, noOfD, noOfO){
  
clust <- agnes(dM, diss = TRUE, metric = Metric) # cluster according to dissim. 
branches <- cutree(clust,2)  # get separation for two clusters

d <- c(rep(1,noOfD))
nd <- c(rep(2,noOfO))
c <- c(d,nd)

adj.Rand <- adjustedRandIndex(branches, c)

return(adj.Rand)
}




round.row <- function(tab,start,fin,dec){
  
  for (i in start:fin){
    
    for (j in 1:dim(tab)[1]){
      
      tab[j,i] <- round(tab[j,i],dec) 
      
    }
  }
  return(tab)
}

