#1.b

##Show the first 3 and last 3 observations (rows) of the R's standard dataset mtcars
head(mtcars, 3)

tail(mtcars, 3)

##Find in this dataset the car with the maximal Horsepower
max(mtcars$hp)

mtcars[mtcars$hp == max(mtcars$hp),]

##Calculate the Frequency Table for the Number of forward gears
table(mtcars$gear)

##Plot the BarPlot, PieChart, Line Graph and the Polygon Plot for the gear Variabl
barplot(table(mtcars$gear))
pie(table(mtcars$gear))
plot(table(mtcars$gear))
plot(table(mtcars$gear), type = "l")

#1.c

## Write an R code (function, if you know how to write functions in R) that will make the Relative Frequency Table for a given 1D numerical Dataset x
freq_function <- function (x)
{
  d = as.data.frame(table(x))
  z = d$Freq/length(x)
  hist(x,freq = z)
}

x = c(1,2,3,3,5,3,4,5,6,2)
freq_function(x)

#2.c
res <- sample(1:6, 100, replace = T)
table(res)
p <- ecdf(res)
plot(p)
barplot(table(res))

#2.d
smp <- rexp(1000, rate = 0.3)
plot(ecdf(smp), col = "green", lwd = 2, xlim= c(0,7), ylim = c(0,1))
par(new = TRUE)
plot(p,xlim= c(0,7), ylim = c(0,1))

#3.b
help(islands)
str(islands)
head(islands)
df <- data.frame(islands, islands < 200000)
hist(df$islands[df$islands < 200000])
hist(df$islands[df$islands < 200000], freq = FALSE, xlim= c(0,17000), ylim = c(0,4e-04))
par(new = TRUE)
plot(density(df$islands[df$islands < 200000]), col = "red", lwd = 3, xlim= c(0,17000), ylim = c(0,4e-04)) 

#3.c
help(rweibull)
n = 1000
s <- rweibull(n, shape = 2)
hist(s, freq = FALSE, xlim= c(0,3), ylim = c(0,1))
par(new = TRUE)
plot(dweibull(n, shape=2), col = "red", lwd = 3, xlim= c(0,3), ylim = c(0,1))

#4.a
a <- c(0,3,4,5,-1)
b <- c(2,-1,2,5,2)
plot(a,b)

#4.b
pressure
plot(pressure$temperature, pressure$pressure)

#5
aapl <- read.csv("file:///C:/Users/ASUS/Downloads/AAPL.csv")
hist(aapl$Adj.Close)
## right skewed dual mode distribution with the highest freq of 40-50 