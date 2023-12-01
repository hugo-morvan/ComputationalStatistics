################### lab 5

########## Ex. 2

rm(list=ls(all.names=T))


data <- read.csv("prices1.csv", sep=";")
summary(data)
dim(data)

### 1)

price <- data$Price
sqft <- data$SqFt

plot(price, sqft)

fit1 <- lm(sqft ~ price)
lines(price, fit1$fitted.values, col="red", type="l", lwd=1)

# answer a:

### 2)

c_start <- 1
var2 <- sqft - c_start  ## create a var for the last part of lin reg
### calculate the residuals for the RSS calculus to give it to optim function

optim(lm(price ~ 1 + sqft + (sqft - c_start)*as.numeric(sqft>rep(c_start, nrow(data)))))






