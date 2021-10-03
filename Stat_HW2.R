## 1.d

x <- c(2,2,2,5,3,0,0,2,3,5)
mean(x, trim = 0.25)


wins <- function(x,p) {
  ((p+1)*x[1]+sum(x[2:length(x)-1])+(p+1)*x[length(x)])/length(x)
}
wins(x,2)

## 2.b

# 1
MAD <- function(x) {
  mean <- mean(x)
  deviations <- x - mean
  mean(abs(deviations))
}

MAD(cars$speed)
MAD(cars$dist)

# 2
MAD2 <- function(x) {
  median <- median(x)
  deviations <- x - median
  mean(abs(deviations))
}

MAD2(cars$speed)
MAD2(cars$dist)

## 3.c
boxplot(Petal.Width~Species, data=iris, horizontal = T)

# we can see that the highest widths has viginica while 25% of virginica irises have similar petal width with versicolor
#, which in its place has a median width of ~1.3. Setosa irises have the smallest petal widths with two outlier observations, 
# 50% of setosa observations have ~0.25-0.35 petal width, etc.

## 4.b
quant <- function(x,y) {
  quantile(x,y)
}
quant(x,0.21)

## 5.c.1
quant(dnorm(x, mean=2, sd = 3),0.1)

## 5.c.2
quantile(dexp(x, rate = 2.5),c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))

## 6.b.1
y <- rexp(300,rate=3)
z <- rexp(400,rate=0.2)
qqplot(y,z)

## 6.b.2
qqexp <- function(x) {
  qqplot(x,rexp(10000,rate=1))
}
qqexp(x)

qqunif <- function(x) {
  qqplot(x,runif(10000,0,1))
}
qqunif(x)