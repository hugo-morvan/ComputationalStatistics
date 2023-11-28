################### lab 4 

########## Ex. 1

### a)

f <- function(x){
  stopifnot(x>0)
  return(x^5*exp(-x))
}

X <- seq(0.01,15,0.01)
plot(X, f(X), type="l", lwd=3) # f(x) distribution

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
cat("acceptance rate =", accept / N)

hist(chain)
plot(chain, type="l")  

# We observe from this plot that this method doesn't really converge and
# the majority of iterations are rejected (acceptance rate = 0.0309).
# Furthermore, a burn-in period is hard to detect / does not even look 
# useful in this scenario, as this method is not really giving us good results.

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
cat("acceptance rate =", accept2 / N)

hist(chain2)
plot(chain2[1:100], type="l")

### c)

starting_point3 <- 3
N <- 1E4
set.seed(12345)

proposal_dist3 <- function(Xt){
  return(rexp(1, Xt^-1))
}

chain3 <- starting_point3
accept3 <- 0

for (i in 1:N){
  
  x <- chain3[length(chain3)]
  # candidate point x_star from g
  x_star <- proposal_dist3(x)
  
  # MH ratio
  R     <- (f(x_star) * dexp(x, x_star^-1)) / 
    (f(x) * dexp(x_star, x^-1))
  ap    <- runif(1)
  if (ap < R){
    chain3 <- c(chain3, x_star)
    accept3 <- accept3 + 1
  } else chain3 <- c(chain3, x) 
}

mean(chain3)
cat("acceptance rate =", accept3 / N)

hist(chain3)
plot(chain3, type="l")

## d)

# Looking at the histograms, it is clear how the second and 
# third methods generate more reasonable results, both with 
# better acceptance rates than the first method. The first method seems to 
# generate too many samples with low values (< 4). The other two seem to generate
# fair results, compared to the distribution of f(x). They produce very similar
# results with slightly different acceptance rates, the Chi Squared distribution
# seems to be the best proposal distribution to choose for our case. A burn-in 
# period of 30 was chosen (mainly for method b) and c) as method a) does not
# get any better even with this burn-in period).

par(mfrow=c(3,1))
plot(chain[-c(1:30)], type="l")
plot(chain2[-c(1:30)], type="l")
plot(chain3[-c(1:30)], type="l")

cat("Method a) acceptance rate =", accept / N, "\n")
cat("Method b) acceptance rate =", accept2 / N, "\n")
cat("Method c) acceptance rate =", accept3 / N, "\n")

par(mfrow=c(3,1))
hist(chain[-c(1:30)], main = "Histogram of part a)", xlab = "x values")
hist(chain2[-c(1:30)], main = "Histogram of part b)", xlab = "x values")
hist(chain3[-c(1:30)], main = "Histogram of part c)", xlab = "x values")

par(mfrow=c(1,1))
plot(X, f(X), type="l", lwd=3)

# e)

cat("the expected value of the first method is:", mean(chain[-c(1:30)]), "\n")
cat("the expected value of the second method is:", mean(chain2[-c(1:30)]), "\n")
cat("the expected value of the third method is:", mean(chain3[-c(1:30)]), "\n")

# f)

# The distribution f(x) is a Gamma distribution with parameters
# alpha=6 and beta=1, hence E(x) = alpha/beta = 6.
# The results we obtained are pretty accurate with b) and c) methods,
# but not accurate at all using the first method a).
