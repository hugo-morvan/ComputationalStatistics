################### lab 4 

### a)

f <- function(x){
  stopifnot(x>0)
  return(x^5*exp(-x))
}

x <- seq(0.01,15,0.01)
plot(x, f(x), type="l", lwd=3)

starting_point <- 3
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

hist(chain)
plot(chain, type="l")

### b)

starting_point2 <- 1
N <- 1E4
set.seed(12345)

proposal_dist2 <- function(Xt){
  return(rchisq(1, floor(Xt+1)))
}

chain2 <- starting_point2
accept2 <- 0

for (i in 1:N){
  
  x <- chain2[length(chain2)]
  # candidate point x_star from g
  x_star <- proposal_dist2(x)
  
  # MH ratio
  R     <- (f(x_star) * dchisq(x, floor(x_star + 1))) / 
    (f(x) * dchisq(x_star, floor(x+1)))
  ap    <- runif(1)
  if (ap < R){
    chain2 <- c(chain2, x_star)
    accept2 <- accept2 + 1
  } else chain2 <- c(chain2, x) 
}

chain2
accept2

hist(chain2)
plot(chain2, type="l")

## c)

starting_point3 <- 1
N <- 1E4
set.seed(12345)

proposal_dist3 <- function(Xt){
  return(rchisq(1, floor(Xt+5)))
}

chain3 <- starting_point2
accept3 <- 0

for (i in 1:N){
  
  x <- chain3[length(chain3)]
  # candidate point x_star from g
  x_star <- proposal_dist3(x)
  
  # MH ratio
  R     <- (f(x_star) * dchisq(x, floor(x_star + 5))) / 
    (f(x) * dchisq(x_star, floor(x + 5)))
  ap    <- runif(1)
  if (ap < R){
    chain3 <- c(chain3, x_star)
    accept3 <- accept3 + 1
  } else chain3 <- c(chain3, x) 
}

plot(chain3, type="l")
hist(chain3)

## d)

par(mfrow=c(3,1))
plot(chain, type="l")
plot(chain2, type="l")
plot(chain3, type="l")

# e)

mean(chain)
mean(chain2)
mean(chain3)

# f)

# the distribution f(x) is a Gamma distribution with parameters
# alpha=6 and beta=1, hence E(x) = alpha/beta = 6.
# the results we obtained are pretty accurate with b) and c) methods,
# but not accurate at all using the first method a).
