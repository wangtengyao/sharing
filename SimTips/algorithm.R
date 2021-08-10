library(softImpute)
library(InspectChangepoint)
source('auxiliary.R')
cusum.vector <- function(x){
  ob <- !is.na(x)
  z <- replace(x, !ob, 0)
  n <- length(x)
  leftsum <- cumsum(z)
  rightsum <- leftsum[n] - leftsum
  L <- cumsum(ob)
  R <- L[n] - L
  x.cusum <- (leftsum[-n] / L[-n] - rightsum[-n] / R[-n]) * sqrt(L[-n] * R[-n] / L[n])
  x.cusum[is.nan(x.cusum)] <- 0
  x.cusum
}

cusum <- function(x){
  if (!is.matrix(x)) {
    return(cusum.vector(x))
  } else if (ncol(x)==2){
    return(as.matrix(apply(x, 1, cusum.vector)))
  } else {
    return(t(apply(x, 1, cusum.vector)))
  }
}

get.v.proj <- function(x.cusum, lambda, max_iter=1000, tol=1e-10){
  vhat <- svd(x.cusum)$u[, 1]
  if (sum(abs(x.cusum)) == 0) return(random.UnitVector(nrow(x.cusum)))
  for (iter in 1:max_iter){
    vhat_old <- vhat
    what <- vector.normalise(t(x.cusum) %*% vhat)
    tmp <- x.cusum %*% what
    lambda_tmp <- min(lambda, max(abs(tmp)) - 1e-10)
    vhat <- vector.normalise(vector.soft.thresh(x.cusum %*% what, lambda_tmp))
    if (vector.norm(vhat_old - vhat) < tol) break
  }
  return(as.vector(vhat))
}

locate.change <- function(x, lambda, standardize.series=FALSE, view.cusum=FALSE){
  if (!is.matrix(x)) x <- t(x)
  p <- nrow(x)
  n <- ncol(x)
  if (missing(lambda)) lambda <- sqrt(n*log(p*log(n)))/2
  if (standardize.series) x <- rescale.variance(x)

  x.cusum <- cusum(x)
  lambda <- min(lambda, max(apply(x.cusum, 1, vector.norm)) - 1e-10)

  vhat <- get.v.proj(x.cusum, lambda)

  x.cusum.proj <- colSums(as.vector(vhat) * x.cusum)
  cp <- which.max(abs(x.cusum.proj) * (1 + rnorm(length(x.cusum.proj)) * 1e-10)) # random tie breaking
  if (view.cusum) {
    plot(x.cusum.proj, ylab = "projected cusum", pch = 20)
  }
  attr(cp, 'cusum') <- x.cusum.proj
  attr(cp, 'vhat') <- vhat
  return(cp)
}

imputeInspect <- function(x, lambda, standardize.series=FALSE){
  if (!is.matrix(x)) x <- t(x)
  p <- nrow(x)
  n <- ncol(x)
  if (missing(lambda)) lambda <- sqrt(log(log(n) * p)/2)
  if (standardize.series) x <- rescale.variance(x)

  tmp <- softImpute(x)
  y <- with(tmp, u %*% diag(d) %*% t(v))
  x[is.na(x)] <- y[is.na(x)]
  x.cusum <- InspectChangepoint::cusum.transform(x)
  vhat <- svd(vector.soft.thresh(x, lambda))$u[,1]
  cusum.proj <- colSums(vhat * x.cusum)
  cp <- which.max(abs(cusum.proj))
  attr(cp, 'vhat') <- vhat
  attr(cp, 'cusum') <- cusum.proj
  return(cp)
}

BinSeg <- function(x, s=0, e=ncol(x), depth=1) {
  if (e - s <= 1)
    return(NULL)

  tmp <- locate.change(x[, (s + 1):e])
  cp <- s + as.numeric(tmp)
  max.val <- max(abs(attr(tmp, 'cusum')))


  ret <- setNames(c(cp, max.val, depth), c('cp', 'max.val', 'depth'))

  return(rbind(BinSeg(x, s, cp, depth + 1),
               ret,
               BinSeg(x, cp, e, depth + 1)))
}

genData <- function(n, p, k, z, vartheta, q1, q2){
  q <- rep(c(q1, q2), c(k, p-k))
  x <- InspectChangepoint::single.change(n, p, k, z, vartheta/sqrt(k), shape=0)$x
  mask <- matrix(random.bernoulli(p * n, rep(q, n)) == 0, p)
  x[mask] <- NA
  return(x)
}