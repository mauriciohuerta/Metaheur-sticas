Cruza <- function(Pop, params){
  nPop <- nrow(Pop)
  indexMatrix <- matrix(sample(0:1, size=nPop*params$M/2, replace = T), nrow=nPop/2)
#  indexMatrix_2 <- (indexMatrix_1-1)*(-1)
  Pop_1 <- Pop[1:(nPop/2),]
  Pop_2 <- Pop[(nPop/2+1):nPop,]
  id1 <- which(indexMatrix == 1, arr.ind = TRUE)
  id2 <- which(indexMatrix == 0, arr.ind = TRUE)
  newPop_1 <- Pop_1
  newPop_1[id1] <- Pop_2[id1]
  newPop_2 <- Pop_2
  newPop_2[id2] <- Pop_1[id2]
  newPop <- rbind(newPop_1, newPop_2)
  return(newPop)
}
