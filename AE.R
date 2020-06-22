AE <- function(nPop, pMut, max_iter, params, pi, Verbose = FALSE){
  PobInicial <- matrix(sample(0:params$N, size=nPop*params$M, replace = T), nrow=nPop)
  Poblacion <- as.data.frame(PobInicial)
  bestFO <- Inf
  bestGen <- NULL
  best <- best.i <- NULL
  for(i in 1:max_iter){
  PobSel <- Seleccion(Pop = Poblacion, pi = pi, params = params)
  if(PobSel$bestFO < bestFO){
    bestFO <- PobSel$bestFO
    bestGen <- PobSel$bestGen
  }
  if(Verbose){
    cat("iter ", i, ". La mejor solución encontrada es: ",bestFO, "\n")
  }
  PobCross <- Cruza(Pop = PobSel$Pop, params = params)
  Poblacion <- Mutacion(PobCross, pMut=0.1, params = params)
  best[i] <- bestFO
  best.i[i] <- PobSel$bestFO
  }
  return(list(bestFO = bestFO, bestGen = bestGen, Hist_Best.i = best.i, Hist_Best = best))
}
