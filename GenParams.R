GenParams <- function(N, M, seed = F, plot = T, dist = "beta", pars = list(shape1 = 2, shape2 = 2), ...){
  if(is.numeric(seed)){
    set.seed(seed)
  }
  pars$n <- N + M
  simX   <- do.call(paste0("r",dist), args = pars)
  simY   <- do.call(paste0("r",dist), args = pars)
  if(plot){
    plot(simX, simY,
         col  = c(rep(2,N),rep(4,M)),
         xlim = c(0,1),
         ylim = c(0,1),
         ...)
  }
  FC <- floor(runif(N, min = 5000, max = 15001))
  OC <- floor(runif(N, min = 5000, max = 10001))
  LT <- floor(runif(N, min = 1,    max = 6))
  d  <- floor(runif(M, min = 50,   max = 101))
  v  <- floor(runif(M, min = 500,  max = 1501))
  HC    <- rep(100, N)
  ICAP  <- rep(1200, N)
  Qmax  <- rep(600, N)
  alpha <- beta <- 2.5
  pos   <- data.frame(x = simX, y = simY)
  dist  <- round(100000*as.matrix(dist(pos, diag=T))[(N+1):(N+M), 1:N])
  params <- list(N     = N,
                 M     = M,
                 FC    = FC,
                 OC    = OC,
                 HC    = HC,
                 LT    = LT,
                 d     = d,
                 v     = v,
                 ICap  = ICAP,
                 Qmax  = Qmax,
                 alpha = alpha,
                 beta  = beta,
                 AC    = dist)
  class(params) <- "EII868"
  return(params)
}
