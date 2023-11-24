### lab 4 ex 1

f <- function(x){
  stopifnot(x>0)
  return(x^5*exp(-x))
}

x <- seq(0.01,15,0.01)
plot(x, f(x), type="l", lwd=3)

starting_point <- 1
N <- 1E4
set.seed(12345)

proposal_dist <- function(Xt){
  return(rlnorm(1, Xt, 1))
}

chain <- starting_point
accept <- 0

for (i in 1:N){
  
  x <- chain[length(chain)]
  # candidate point x_star from g
  x_star <- proposal_dist(x)
  
  # MH ratio
  R     <- (f(x_star) * dlnorm(x, x_star, 1)) / 
            (f(x) * dlnorm(x_star, x, 1))
  ap    <- runif(1)
  if (ap < R){
    chain <- c(chain, x_star)
    accept <- accept + 1
  } else chain <- c(chain, x) 
}

chain 
accept

plot(chain, type="l")

### ex 2

starting_point <- 1
N <- 1E4
set.seed(12345)

proposal_dist2 <- function(Xt){
  return(rchisq(1, floor(Xt+1)))
}

chain <- starting_point
accept <- 0

for (i in 1:N){
  
  x <- chain[length(chain)]
  # candidate point x_star from g
  x_star <- proposal_dist2(x)
  
  # MH ratio
  R     <- (f(x_star) * dlnorm(x, x_star, 1)) / 
    (f(x) * dlnorm(x_star, x, 1))
  ap    <- runif(1)
  if (ap < R){
    chain <- c(chain, x_star)
    accept <- accept + 1
  } else chain <- c(chain, x) 
}

chain
accept

plot(chain, type="l")

