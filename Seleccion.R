Seleccion <- function(Pop, pi, params){
  Poblacion <- Pop
  Poblacion$fo <- apply(Poblacion, MARGIN = 1, FUN = FO, pi = pi, params = params)
  Poblacion <- Poblacion[order(Poblacion$fo, decreasing = TRUE),]
  best_fo <- Poblacion$fo[nrow(Poblacion)]
  best_genoma <- Poblacion[nrow(Poblacion),-ncol(Poblacion)]
  npop <- nrow(Pop)
  idSel <- sample(1:npop, size = npop, replace = TRUE, prob = 2*(1:npop)/(npop*(npop+1)))
  newPop <- Poblacion[idSel,-ncol(Poblacion)]
  output <- list(Pop = newPop, bestFO = best_fo, bestGen = best_genoma)
  return(output)
}
