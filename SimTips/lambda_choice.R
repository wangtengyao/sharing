source('algorithm.R')
library(putils)
library(digest)

machine = paste0(Sys.info()["nodename"])
println('machine ', machine, ' started.')
sink(paste0('outfiles/', machine, '.out'))

seed <- strtoi(substr(digest(machine), 1, 6), 16)

df <- sim.params(n=1000, p=500, k=3, z=400, vartheta=2, 
                 q1=c(0.1, 0.2, 0.3, 0.4, 0.5), 
                 q2=c(0.1, 0.2, 0.3, 0.4, 0.5), 
                 a=seq(0, 2, by=0.1), rep=1:3)

for (i in 1:nrow(df)){
  # get simulation parameter from ith row of df
  bunch(n, p, k, z, vartheta, q1, q2, a) %=% df[i, 1:8]
  
  # generate partially observed data matrix 
  x <- genData(n, p, k, z, vartheta, q1, q2)

  # estimate changepoint
  cp <- locate.change(x, lambda=sqrt(n*log(p*n))*a)
  
  # obtain projection direction estimator
  vhat <- attr(cp, 'vhat')
  
  # compute angle to the oracle direction
  v.oracle <- vector.normalise(rep(c(1,0), c(k, p-k)))
  angle <- acos(abs(sum(vhat * v.oracle))) / pi * 180
  
  # output result
  print(show.params(machine, n, p, k, z, vartheta, q1, q2, a, cp, angle))
}

sink()



