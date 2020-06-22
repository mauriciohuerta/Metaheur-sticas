### Main

source("GenParams.R")
source("FO.R")
source("Seleccion.R")
source("Cruza.R")
source("Mutacion.R")
source("AE.R")

nrep <- 30

set.seed(12345)
seeds <- floor(runif(nrep, min=100000, max=999999))

resultados <- list()
contador <- 0
for(s in seeds){
  pars <- GenParams(N = 5, M = 10, plot = F, seed = s)
  for(pbb in c(0.1, 0.25, 0.5)){
    for(tPop in c(50, 100, 200)){
      t <- proc.time()
      res <- AE(nPop = tPop, pMut = pbb, max_iter = 150, params = pars, pi = 1, Verbose = F)
      res$time <- proc.time()-t
      contador <- contador + 1
      resultados[[contador]] <- res
      rm(t, res)
      print(contador)
    }
  }
}

save(resultados, file = "resultados.RData")

P  <- rep(c(50, 100, 200), 90)
pm <- rep(c(0.1, 0.2, 0.5), each = 3, times = 30)
id <- rep(seeds, each= 9)

factores <- data.frame(id = id, pm = pm, P = P)

sol <- time <- NULL
for(i in 1:contador){
  sol[i]  <- resultados[[i]]$bestFO
  time[i] <- resultados[[i]]$time[3]
}

data <- cbind(factores, sol, time)

aggregate(data$sol, by=list(data$pm, data$P), FUN=mean)
aggregate(data$sol, by=list(data$pm, data$P), FUN=sd)

aggregate(data$time, by=list(data$pm, data$P), FUN=mean)
aggregate(data$time, by=list(data$pm, data$P), FUN=sd)

library(ggplot2)
ggplot(data =data, aes(x=factor(P),y=sol, fill=factor(pm))) +
  geom_boxplot() +
  labs(fill = expression("p"[m]), x = "P", y="Valor\n(función objetivo)")  + theme_bw()

ggplot(data =data, aes(x=factor(P),y=time, fill=factor(pm))) +
  geom_boxplot() +
  labs(fill = expression("p"[m]), x = "P", y="Tiempo (segundos)") + theme_bw()

mod1 <- aov(sol ~ factor(P) + factor(pm), data = data[1:9,])
summary(mod1)

mod2 <- aov(time ~ factor(P) + factor(pm), data = data[1:9,])
summary(mod2)

g <- ggplot()+
  ylim(0,2.5e+06)+
  geom_line(aes(x=1:150, y=resultados[[1]]$Hist_Best), alpha = 1)
for(i in 2:270){
  g <- g + geom_line(aes(x=1:150, y=resultados[[i]]$Hist_Best), alpha = 1)
}
g