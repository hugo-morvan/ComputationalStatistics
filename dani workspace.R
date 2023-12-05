################### lab 5

########## Ex. 2

rm(list=ls(all.names=T))

data <- read.csv("prices1.csv", sep=";")

### 1)

price <- data$Price
sqft <- data$SqFt

plot(sqft, price)
fit1 <- lm(Price ~ SqFt, data)
lines(sqft, fit1$fitted.values, col="darkred", type="l", lwd=1)

# answer a:

### 2)

fit2 <- function(c, data){
  data$new <- rep(0,110)
  for (i in 1:nrow(data)){
    if (data[i,2] > c) data[i,5] <- data[i,2] - c
    else data[i,5] <- 0
  }
  lm2 <- lm(Price ~ SqFt + new, data)
  sum(lm2$residuals^2)
}

RSS_opt <- optim(2000, fit2, data=data, method="Brent", lower=0, upper=4500)

### 3)



