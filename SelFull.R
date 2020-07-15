SelElitismo <- function(Pop, pi, params){
  Poblacion    <- Pop
  Poblacion$fo <- apply(Poblacion,
                        MARGIN = 1, 
                        FUN    = FO, 
                        pi     = pi, 
                        params = params)
  Poblacion   <- Poblacion[order(Poblacion$fo, decreasing = TRUE),]
  best_fo     <- Poblacion$fo[nrow(Poblacion)]
  best_genoma <- Poblacion[nrow(Poblacion), -ncol(Poblacion)]
  npop        <- nrow(Pop)
  idSel       <- sample(rep(((npop/2)+1):npop, each = 2), 
                        size    = npop, 
                        replace = FALSE)
  newPop      <- Poblacion[idSel, -ncol(Poblacion)]
  output      <- list(Pop     = newPop, 
                      bestFO  = best_fo, 
                      bestGen = best_genoma)
  return(output)
}

SelRangos <- function(Pop, pi, params){
  Poblacion    <- Pop
  Poblacion$fo <- apply(Poblacion,
                        MARGIN = 1, 
                        FUN    = FO, 
                        pi     = pi, 
                        params = params)
  Poblacion   <- Poblacion[order(Poblacion$fo, decreasing = TRUE),]
  best_fo     <- Poblacion$fo[nrow(Poblacion)]
  best_genoma <- Poblacion[nrow(Poblacion), -ncol(Poblacion)]
  npop        <- nrow(Pop)
  idSel       <- sample(1:npop, 
                        size    = npop, 
                        replace = TRUE, 
                        prob    = 2*(1:npop)/(npop*(npop+1)))
  newPop      <- Poblacion[idSel, -ncol(Poblacion)]
  output      <- list(Pop     = newPop, 
                      bestFO  = best_fo, 
                      bestGen = best_genoma)
  return(output)
}

SelTorneo <- function(Pop, pi, params){
  Poblacion    <- Pop
  Poblacion$fo <- apply(Poblacion,
                        MARGIN = 1, 
                        FUN    = FO, 
                        pi     = pi, 
                        params = params)
  npop        <- nrow(Pop)
  best_fo     <- Poblacion$fo[which.min(Poblacion$fo)]
  best_genoma <- Poblacion[which.min(Poblacion$fo), -ncol(Poblacion)]
  ID4torneo   <- sample(1:npop, 
                        size    = 4*npop, 
                        replace = TRUE)
  FO4torneo   <- Poblacion$fo[ID4torneo]
  IDmatriz    <- matrix(ID4torneo, 
                        nrow  = npop)
  FOmatriz    <- matrix(FO4torneo, 
                        nrow  = npop)
  winners     <- apply(FOmatriz, 
                       MARGIN = 1, 
                       FUN    = which.min)
  winners     <- as.matrix(cbind(1:npop, winners))
  IDwinners   <- IDmatriz[winners]
  newPop      <- Poblacion[IDwinners, -ncol(Poblacion)]
  output      <- list(Pop     = newPop, 
                      bestFO  = best_fo, 
                      bestGen = best_genoma)
  return(output)
}