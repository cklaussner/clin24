



results <- matrix(0, nrow = length(cv),ncol=3)
rownames(results) <- names(cv)
results[,1] <- "&" 
results[,3] <- "\\" 

for (i in 1:length(cv)){
  results[i,2] <- round(cv[[i]],3)

}
results <- as.data.frame(results)





############### two results


results <- matrix(0, nrow = length(cv.uni),ncol=5)
rownames(results) <- names(cv.uni)
results[,1] <- "&" 

results[,3] <- "&" 
results[,5] <- "\\" 

for (i in 1:length(cv1)){
  results[i,2] <- round(cv.uni[[i]],3)
  results[i,4] <- round(cv.posbi[[i]],3)
}
results <- as.data.frame(results)








############### three results


results <- matrix(0, nrow = length(cv.uni),ncol=7)
rownames(results) <- names(cv.uni)
results[,1] <- "&" 

results[,3] <- "&" 

results[,5] <- "&" 

results[,7] <- "\\" 

for (i in 1:length(cv.uni)){
  results[i,2] <- round(cv.uni[[i]],2)
  results[i,4] <- round(cv.posbi[[i]],2)
  results[i,6] <- round(cv.postri[[i]],2)
}
results <- as.data.frame(results)




######################### features
features <- matrix(0, nrow = 20,ncol=5)
rownames(features) <- c(1:20)

features[,1] <- "&" 
features[,3] <- "&" 
features[,5] <- "\\" 

for (i in 1:20){
  features[i,2] <- d[i]
  features[i,4] <- c[i]
}
features <- as.data.frame(features)






############################## 3 pair features


features <- matrix(0, nrow = 20,ncol=13)
rownames(features) <- c(1:20)

features[,1] <- "&" 
features[,3] <- "&" 
features[,5] <- "&" 
features[,7] <- "&" 
features[,9] <- "&" 
features[,11] <- "&" 

features[,13] <- "\\" 

for (i in 1:20){
  features[i,2] <- d[i]
  features[i,4] <- c[i]
  features[i,6] <- d2[i]
  features[i,8] <- c2[i]
  features[i,10] <- d3[i]
  features[i,12] <- c3[i]
}
features <- as.data.frame(features)


















