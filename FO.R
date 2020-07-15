############################################################################
############################################################################
##############                                               ###############
##############                      FO                       ###############
##############   Funcion para calcular la funcion objetivo   ###############
##############                                               ###############
############################################################################
############################################################################
###                                                                      ###
### Inputs:                                                              ###
###                                                                      ###
############################################################################
### cromosoma : Vector de enteros. Representacion de la solucion como    ###
###             un cromosoma.                                            ###
### pi        : Numerico. Especifica la proporcion de demanda minima que ###
###             se desea capturar. Debe estar entre 0 y 1.               ###
### params    : Objeto de clase EII868. Lista que contiene los           ###
###             valores de lso parametros.                               ###
############################################################################
############################################################################

FO <- function(cromosoma, pi, params){
  if(class(params)!="EII868"){
    stop("La clase del objeto params es diferente a EII868")
  }
  attach(params)
  if(sum(cromosoma)==0){
    valor <- 0
    D     <- 0
  } else{
    id.x <- (1:N) %in% unique(cromosoma)
    id.y <- which(cromosoma != 0)
    X <- as.numeric(id.x)
    D <- aggregate(cbind(cromosoma, d), by=list(cromosoma), FUN = sum)
    D <- sum(Dq <- D$d[D$Group.1!=0])
    V <- aggregate(cbind(cromosoma, v), by=list(cromosoma), FUN = sum)
    V <- sum(V$v[V$Group.1!=0])
    Q <- sqrt(2*OC[id.x]*D/HC[id.x])
    f1 <- sum(FC*X)
    posiciones <- cbind(seq_along(cromosoma), cromosoma)[id.y,]
    f2 <- sum(AC[posiciones]*cromosoma[id.y])
    f3 <- sum(OC[id.x]*D/Q)
    f4 <- sum(HC[id.x]*Q/2)
    f5 <- sum(HC[id.x]*qnorm(.95)*sqrt(LT[id.x])*sqrt(V))
    valor <- f1 + f2 + f3 + f4 + f5
  }
  penalizacion <- 9e+100^(as.numeric(D < (pi*sum(d)))) - 1
  detach(params)
  valor <- valor + penalizacion
  return(valor)
}
