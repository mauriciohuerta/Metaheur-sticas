Mutacion <- function(Pop, pMut, params){
  nPop <- nrow(Pop)
  mutaciones <- matrix(sample(0:params$N, size=nPop*params$M, replace = T), nrow=nPop)
  indexMatrix <- matrix(sample(0:1, size=nPop*params$M, replace = T, prob = c(1-pMut, pMut)), nrow=nPop)
  id <- which(indexMatrix == 1, arr.ind = TRUE)
  PobMutacion <- Pop
  PobMutacion[id] <- mutaciones[id]
  row.names(PobMutacion) <- NULL
  return(PobMutacion)
}
