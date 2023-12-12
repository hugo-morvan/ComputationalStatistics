################### lab 5

########## Ex. 2

rm(list=ls(all.names=T))

data <- read.csv("prices1.csv", sep=";")

## 1)

price <- data$Price
sqft <- data$SqFt

plot(sqft, price)
fit1 <- lm(Price ~ SqFt, data)
lines(sqft, fit1$fitted.values, col="darkred", type="l", lwd=1)

# A straight line seems to summarize the data pretty well.

## 2)

fit2 <- function(c, data){
  data$new <- rep(0,110)
  for (i in 1:nrow(data)){
    if (data[i,2] > c) data[i,5] <- data[i,2] - c
    else data[i,5] <- 0
  }
  lm2 <- lm(Price ~ SqFt + new, data)
  sum(lm2$residuals^2)
}

RSS_opt <- optim(2000, fit2, data=data, method="L-BFGS-B", lower=0, upper=4500)
RSS_opt$par
RSS_opt$value

## 3)

library(boot)

c_val <- c()

for (i in 1:2E3){
  if (i%%500==0) cat(i, "\r")
  id <- sample(1:110, 110, replace = TRUE)
  data_boot <- data[id,]
  c_opt <- optim(2000, fit2, data=data_boot, method="L-BFGS-B", lower=0, upper=4500)   
  c_val <<- c(c_val, c_opt$par)
}

hist(c_val)

2 * RSS_opt$par - sum(c_val) / 2000      # bootstrap bias-correction
sum((c_val - mean(c_val))^2) / (2000-1)  # variance of c

opt_boot <- function(data, id){
  id <- sample(1:110, 110, replace = TRUE)
  data_boot2 <- data[id,]
  c_opt2 <- optim(2000, fit2, data= data_boot2, method = "L-BFGS-B", lower = 0, upper = 4500)   
  c_opt2$par
}

boot_fun <- boot(data = data, opt_boot, R = 2E3)
boot.ci(boot_fun)
plot(boot_fun)

# We seem to obtain two Gaussian-ish shaped distributions.

## 4)

jack_c <-c()

# new function for the jackknife function with 109 rows instead of 110
fit3 <- function(c, data){
  data$new <- rep(0,109)
  for (i in 1:nrow(data)){
    if (data[i,2] > c) data[i,5] <- data[i,2] - c
    else data[i,5] <- 0
  }
  lm2 <- lm(Price ~ SqFt + new, data)
  sum(lm2$residuals^2)
}

for (i in 1:110){
  id <- sample(1:110, 110, replace = TRUE)
  data_boot3 <- data[id,]
  data_jack <- data_boot3[-i,]
  c_opt <- optim(2000, fit3, data=data_jack, method = "L-BFGS-B", lower = 0, upper = 4500)   
  jack_c <<- c(jack_c, c_opt$par)
}

jack_c
var(jack_c)

# (The values we obtain are not accurate, we will wait for feedback to
# understand what we did wrong.)

## 5) 

################## LAB 6 ###########################à

rm(list=ls(all.names = T))

data <- read.csv2("censoredproc.csv")

### 1)

par(mfrow=c(1,3))

# histogram of all data
hist(as.numeric(data[,1]), freq=F)

# histogram of data with cens=1
hist(as.numeric(data[which(data[,2]==1),1]), freq=F)

# histogram of data with cens=2
hist(as.numeric(data[which(data[,2]==2),1]), freq=F)

min(as.numeric(data[which(data[,2]==1),1]))
min(as.numeric(data[which(data[,2]==2),1]))

### ANSWER: 
# Yes, they both look like exponential distributions.

### 2)

max(as.numeric(data[which(data[,2]==1),1]))
min(as.numeric(data[which(data[,2]==1),1]))

max(as.numeric(data[which(data[,2]==2),1]))
min(as.numeric(data[which(data[,2]==2),1]))

library(ReIns)

x <- seq(0,10, 0.01)
plot(x, dexp(x-2,1), type="l")
lines(x, dexp(x), col="red")

