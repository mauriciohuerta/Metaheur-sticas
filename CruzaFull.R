CruzaUniforme <- function(Pop, params){
  nPop <- nrow(Pop)
  indexMatrix <- matrix(sample(0:1, 
                               size    = nPop*params$M/2, 
                               replace = T), 
                        nrow = nPop/2)
  Pop_1             <- Pop[1:(nPop/2),]
  Pop_2             <- Pop[(nPop/2+1):nPop,]
  id1               <- which(indexMatrix == 1, arr.ind = TRUE)
  id2               <- which(indexMatrix == 0, arr.ind = TRUE)
  newPop_1          <- Pop_1
  newPop_1[id1]     <- Pop_2[id1]
  newPop_2          <- Pop_2
  newPop_2[id2]     <- Pop_1[id2]
  newPop            <- rbind(newPop_1, newPop_2)
  row.names(newPop) <- NULL
  return(newPop)
}


Cruza1punto <- function(Pop, params){
  npop   <- nrow(Pop)
  idCuts <- sample(1:(params$M-1), 
                   size    = npop/2,
                   replace = T)
  pop1              <- Pop[1:(npop/2),]
  pop2              <- Pop[((npop/2)+1):npop,]
  idrows            <- rep(1:(npop/2), idCuts)
  idcols            <- unlist(mapply(seq_len, idCuts))
  index             <- cbind(idrows, idcols)
  newPop1           <- pop1
  newPop1[index]    <- pop2[index]
  newPop2           <- pop2
  newPop2[index]    <- pop1[index]
  newPop            <- rbind(newPop1, newPop2)
  row.names(newPop) <- NULL
  return(newPop)
}

Cruza2puntos <- function(Pop, params){
  npop   <- nrow(Pop)
  IDcuts <- t(apply(t(mapply(FUN      = sample,
                             size     = rep(2,(npop/2)), 
                             replace  = F,
                             MoreArgs = list(x=1:(params$M-1)))), 
                    MARGIN = 1, 
                    FUN    = sort))
  pop1              <- Pop[1:(npop/2),]
  pop2              <- Pop[((npop/2)+1):npop,]
  idrows            <- rep(1:(npop/2), (IDcuts[,2]-IDcuts[,1]))
  idcols            <- unlist(mapply(seq, (IDcuts[,1]+1), IDcuts[,2]))  
  index             <- cbind(idrows, idcols)
  newPop1           <- pop1
  newPop1[index]    <- pop2[index]
  newPop2           <- pop2
  newPop2[index]    <- pop1[index]
  newPop            <- rbind(newPop1, newPop2)
  row.names(newPop) <- NULL
  return(newPop)
}