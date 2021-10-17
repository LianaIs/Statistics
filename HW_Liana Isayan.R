install.packages("wooldridge")
library("wooldridge")

data(ceosal2)

#1 Find the average salary and the average tenure in the sample
names(ceosal2)

mean_T <- mean(ceosal2$ceoten)
mean_S <- mean(ceosal2$salary)

#2 How many CEOs are in their first year as CEO (that is, ceoten = 0)? What is the longest tenure as a CEO?
  
if(ceosal2$ceoten = 0)
  length(ceosal2$ceoten)

max(ceosal2$ceoten)

#3 Estimate the simple regression model:

summary(lm(log(ceosal2$salary) ~ ceosal2$ceoten))

# we have a very small R^2 meaning there may be better explaining variables for CEO salary; t value for b1 is also small
# one more year being a CEO increases the salary by 0.009724% 

#4 Plot the line of linear regression and scatter plot
x <- ceosal2$ceoten
y <- log(ceosal2$salary)

library(tidyr)
library(ggplot2)
cex <- 2
p<-ggplot(tibble(x,y), aes(x=x,y=y))+geom_point(cex = cex)+geom_smooth(se=FALSE, method = lm)
p<-p+ xlab("Years as a CEO")+ylab("Salary") 
p<-p+ geom_segment(aes(x=x, y=y, xend=x, yend=predict(lm(y~x))), col="red")
p


#5_6 Based on this regression calculate b0 and b1 & compare with lm results

b1.hat <- cov(y,x)/var(x)
b0.hat <- mean(log(ceosal2$salary)) - b1.hat * mean_T
c(b0.hat, b1.hat)
coef(lm(log(ceosal2$salary) ~ ceosal2$ceoten))

#7_8 Based on this regression calculate SE of intercept and SE of slope & compare with lm results

yfit <- b0.hat + b1.hat * x # fitted values
e <- y - yfit # residuals
Sxy <- sum((y-mean(log(ceosal2$salary))) * (x-mean(x)))
Sxx <- sum((x-mean(x))^2)
n <- length(y)
sig.hat <- sqrt(sum(e^2)/(n-2))
s1 <- sig.hat * sqrt(n)/sqrt((n*sum(x^2)-sum(x)^2))
s1
s0 <- sig.hat * sqrt(sum(x^2)/(n*sum(x^2)-sum(x)^2))
s0

# numerator <- sqrt(sum((y - yfit)^2)/(n-2)) 
# denominator <- sqrt(sum((x - mean(x))^2))
# numerator/denominator 

coef(summary(lm(log(ceosal2$salary) ~ ceosal2$ceoten)))[, 2]

#9 Based on this regression calculate standard R square & show that it consistent with results of lm function in R

ESS <- sum(e^2)
RSS <- sum((yfit-mean(log(ceosal2$salary)))^2)
TSS <- sum((y-mean(log(ceosal2$salary)))^2)

R2_1 <- RSS/TSS
R2_2 <- summary(lm(log(ceosal2$salary) ~ ceosal2$ceoten))$r.squared
c(R2_1,R2_2)
