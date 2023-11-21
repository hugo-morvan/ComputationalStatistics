x <- seq(-2,2, 0.01)

cdf <- function(x){
  y <- rep(0,length(x))
  for (i in 1:length(x)){
    if (x[i] < -1){
      y[i] <- 0
    }else if (x[i]<=0){
      y[i] <- x[i]^2/2 + x[i] + 0.5
    }else if (x[i]<=1){
      y[i] <- x[i] - (x[i]^2 /2) + .5
    }else y[i] <- 1
  }
  return(y)
}

plot(x,cdf(x), xlim=c(-1.5, 1.5))

yless <- function(x){
  y <- rep(0, length(x))
  for (i in 1:length(x)){
    if (x[i] < 0){
        y[i] <- 0
      }else if(x[i] > 1){
        y[i] <- 0
      }else y[i] <- 2 - 2*x[i]
    }
  return(y)
}

yless2 <- function(x){
  y <- rep(0, length(x))
  for (i in 1:length(x)){
    if (x[i] < -1){
      y[i] <- 0
    }else if(x[i] > 0){
      y[i] <- 0
    }else y[i] <- 2 + 2*x[i]
  }
  return(y)
}

plot(x,yless(x), xlim=c(-1.5, 1.5), type="l")
plot(x,yless2(x), xlim=c(-1.5, 1.5), type="l")

cdf1 <- function(y){
  return(1 - sqrt(1-y))
}

cdf2 <- function(y){
  return(sqrt(y)-1)
}

zerone <- runif(40000, 0,1)
g <- rbinom(40000, 1, 0.5)

genX <- function(x,g){
  res <- rep(0, length(x))
  for (i in 1:length(x)){
    if (g[i] == 0) res[i] <- cdf1(x[i])
    else res[i] <- cdf2(x[i])
  }
  return(res)
}

m <- genX(zerone,g)
hist(genX(zerone, g))
plot(genX()), )
points(m)



