library(putils) # devtools::install_github('wangtengyao/putils')

rms <- function(v){
  sqrt(mean(v^2))
}

mae <- function(v){
  median(abs(v))
}

rescale.variance <- function (x){
  p <- dim(x)[1]
  n <- dim(x)[2]
  for (j in 1:p) {
    scale <- mad(diff(x[j, ]))/sqrt(2)
    x[j, ] <- x[j, ]/scale
  }
  return(x)
}

