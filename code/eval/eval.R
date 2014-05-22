# evalRD cross-validation

#read.csv('DickensCollins74.cvs', row.names=1)
source("RepDis.R")
source("functions.R")
# Evaluation of Representativeness - Distinctiveness pure and simple 

#source("evalRD.R")


#diff <- evalRD(tab,24,24, 1.8)



# initialisation
sim <- list()
clust.eval <- list()
cross.val <- list()
inter.feat.col <- list()
f1 <- list()
f2 <- list()

a1.feat <- list()
a2.feat <- list()

evalRD <- function(dataset,noOfD,noOfO, alpha){
  
  
  dsize <- dim(dataset)  # set parameters
  num.Docs <- dsize[1]  
  num.Terms <- dsize[2]
  
 # dataset <- as.matrix(check.data(dataset,noOfD,noOfO)) # check that Dickens is first in set
  names <- rownames(dataset)
  
  for(doc in names){ 
    
    print(doc)  # current interation
    
    test.set <- as.matrix(dataset[doc,]) # extract doc for test
    
    # create new matrix with document left out
    
    train.set <- dataset[!names %in% doc, ] 
    
    
    numOfD <- noOfD
    numOfnD <- noOfO
    
    if((substr(doc[1],1,1) == "H")){
      
      numOfD <- noOfD-1
      
    }else{
      if(((substr(doc[1],1,1) != "H"))){
        numOfnD <- noOfO-1
      }
    }
    
    
    diff <- repDis(train.set,numOfD,numOfnD) 
    
    
    # retrieve complete set of features
    f1[[doc]] <- diff$f1
    f2[[doc]] <- diff$f2
    
    f1.c <- diff$f1
    f2.c <- diff$f2
    
    # select best for both
    mu.1 <- mean(f1.c)
    sd.1 <- sd(f1.c)
    f1.red <- as.matrix(f1.c[f1.c > ((mu.1+sd.1)*alpha)])
    
    mu.2 <- mean(f2.c)
    sd.2 <- sd(f2.c)
    f2.red <- as.matrix(f2.c[f2.c > ((mu.2+sd.2)*alpha)])
    
    
    
    
    a1.feat[[doc]] <- as.matrix(f1.red[order(f1.red[,1],decreasing = TRUE ),]) # save set
    a2.feat[[doc]] <- as.matrix(f2.red[order(f2.red[,1],decreasing = TRUE ),])  # save set
    
    
    inter.feat <- intersect(rownames(f1.red), rownames(f2.red)) # get features in both sets
    sim.DO <- dissim.Matrix(dataset,inter.feat) # calculate similarity matrix based on RD.features: Dickens
    cr <- getAdjRand(sim.DO, "complete", noOfD, noOfO) # try with some other metric?
    sim[[doc]]<- sim.DO
    clust.eval[[doc]] <- cr
    inter.feat.col[[doc]] <- inter.feat
    
    
  }
  
  
  #----------- sum up results of cross-validation
  
  
  cross.val[["cv"]] <- clust.eval
  cross.val[["sim"]] <- sim
  cross.val[["inter.feat"]] <- inter.feat.col
  cross.val[["f1.red"]] <- a1.feat
  cross.val[["f2.red"]] <- a2.feat
  cross.val[["f1"]] <- f1
  cross.val[["f2"]] <- f2
  
  
  
  return(cross.val)
  
  
  
  
  
}
