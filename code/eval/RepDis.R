

# version: 11.04.2014 
# Representativeness-Distinctiveness



# arguments:
#
# data: document-term matrix
#
# nSet1: number of docs of author 1
# nSet2: number of docs of author 2
#
# alpha: threshold computed on the full list of combined scores by taking mean and sd over whole set 
# and retaining values > alpha*(mean+sd)


repDis <- function(data,nSet1,nSet2){
  
  
  #separate data into two author sets
  
  set1 <- data[1:nSet1,]
  set2 <- data[(nSet1+1):(nSet1+nSet2), ]
  
  #parameter  
  
  # list of terms
  all.terms <- c(colnames(data))
  
  
  # for normalisation over comparisons 
  norm.Set1 <- (2/ ((nSet1^2) - nSet1))
  norm.Set2 <- (2/ ((nSet2^2) - nSet2))
  
  # should be the same for both directions
  norm.dist <- 1/ ((nSet1)* ((nSet2+nSet1) - nSet1)) 
  
  
  # intialisation
  
  # Set1
  rf <- list()
  #rownames(rfeature) <- all.terms
  rval <- list()
  
  # Set2
  rf2 <- list()
  #rownames(rfeature.2) <- all.terms
  rval2 <- list()
  
  df <- list()
  #rownames(dfeature) <- all.terms
  dval <- list()
  
  
  
  ########## Representativeness: for each set individually: compare features within
  
 #iterate over all terms in matrix
  for (t in all.terms){ 
  
   #print(t)
#     # Set1
    sum.values <- represent(set1,nSet1,t)
    rval[[t]] <- sum.values
    rf[t] <- norm.Set1*sum(sum.values)
    
   
    
    
    # Set2
    sum.values.2 <- represent(set2,nSet2,t)
    rval2[[t]] <- sum.values.2
    rf2[t] <- norm.Set2*sum(sum.values.2)
    
    
    
    ########## Distinctiveness: for each feature: compare both sets to each other
    
    
    sum.dvalues <- distinct(set1,set2,nSet1,nSet2,t)

    dval[[t]] <- sum.dvalues
    df[t] <-  norm.dist*sum(sum.dvalues)
    
    
  }
  
  
 
 ########### Representativeness & Distinctiveness
 
# This is directed analysis, so it has to be done separately for each set

 #new.terms.1 <- union(names(rep.feature),names(dist.feature))
 #new.terms.2 <- union(names(rep2.feature),names(dist.feature))
 
 
 
 # Set1
 
 dist.all <- list()
 f1 <- as.matrix(rep(0, length(1)))

#sometimes there're missing values here - maybe fix this another way?
terms.1 <-intersect(names(dval),names(rval))

for (i in terms.1){
  
  #print(i)
  # sum over all distance vals for feature
    
  dist.all[[i]] <- c(dval[[i]],rval[[i]])
  df.mu <- mean(dist.all[[i]])
  #print(df.mu)
  df.sd <- sd(dist.all[[i]])
  
  f1[i] <- abs( ((df[[i]] -  df.mu)/df.sd) - ((rf[[i]]- df.mu)/df.sd) )
  
}


# Set2


dist.all.2 <- list()
f2 <- as.matrix(rep(0, length(1)))

#sometimes there're missing values here - maybe fix this another way?
terms.2 <-intersect(names(dval),names(rval2))

for (l in terms.2){
  
  #print(l)
  # sum over all distance vals for feature
  
  dist.all.2[[l]] <- c(dval[[l]],rval2[[l]])
  df.mu2 <- mean(dist.all.2[[l]])
 
  df.sd2 <- sd(dist.all.2[[l]])
  #print(df.sd2)
  f2[l] <- abs(((df[[l]] -  df.mu2)/df.sd2) - ((rf2[[l]]- df.mu2)/df.sd2) )
  
}

values <- list()
values[["f1"]] <- f1
values[["f2"]] <- f2


return(values)  
  
}

represent <- function(set,noOfDoc,t){
  
  
  sum.values <- c()
  
  #primary set
  for (j in 1:(noOfDoc-1)){
    
    #if ((set[j,t]) == 0){next}
    
    d.1 <- as.double((set[j,t]))
    
    for (jj in j+1:(noOfDoc-j)){
      
      #if ((set[jj,t]) == 0){next}
      
      d.2 <- as.double((set[jj,t]))
      
      dist.dd <- abs(d.1-d.2)
      sum.values <- c(sum.values,dist.dd)
    }
  }
  
  return(sum.values)  
}


distinct <- function(set1,set2,nSet1,nSet2,t){

sum.values <- c()

for (l in 1:nSet1){
  
  #if ((set1[l,t]) == 0){ next}
  
  d.2 <- as.double((set1[l,t]))
  
  for (ll in 1:nSet2){
    
    #if ((set2[ll,t]) == 0){ next}
    c.2 <- as.double((set2[ll,t]))
    
    dist.dc <- abs(d.2 -c.2)
    sum.values <- c(sum.values,dist.dc)
  }
}
return(sum.values)
}
