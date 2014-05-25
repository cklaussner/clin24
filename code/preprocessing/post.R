



results <- matrix(0, nrow = length(cv),ncol=3)
rownames(results) <- names(cv)
results[,1] <- "&" 
results[,3] <- "\\" 

for (i in 1:length(cv)){
  results[i,2] <- round(cv[[i]],3)

}
results <- as.data.frame(results)





results <- matrix(0, nrow = length(cv),ncol=5)
rownames(results) <- names(cv)
results[,1] <- "&" 

results[,3] <- "&" 
results[,5] <- "\\" 

for (i in 1:length(cv)){
  results[i,2] <- round(cv[[i]],3)
  results[i,4] <- round(cv.pos[[i]],3)
}
results <- as.data.frame(results)






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





