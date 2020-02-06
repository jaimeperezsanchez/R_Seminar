# Set working directory
setwd("/home/jaime/UPM/MLLB/Machine_Learning_Lab/R_Seminar/3_Exercises_to_practice/3_Material_Exercises_to_practice/")

# Ej 1
x <- c(2,5,120,5,8,11)
### Create a vector that contains the indices of x
i <- 1:length(x)
i

# Ej 2
### Create a vector that contains the inverted indices of x
inv_i <- length(x):1
inv_i

# Ej 3
### Create the inverse vector of x 
inv_x <- x[inv_i]
inv_x

# Ej 4
### Add a 7 bewteen the 5 and the 8 (indexes 4 and 5)
x <- c(x[1:4], 7, x[5:length(x)])
x
### Add a 3 at the end of the vector
x <- c(x, 3)
x
### The 120 value is a typo, it should be 12
x[x == 120] <- 12
x

# Ej 5
### Plot a sinusoidal function
x <- seq(0,5,0.01)
y <- sin(x)
plot(x,y)

# Ej 6
### Create a vector of repeated sequences
x <- c(rep(0.5, 4), rep(1.5, 4), rep(3, 4))
x

# Ej 7
x <- c(1,3,5,7,9)
y <- c(2,3,5,7,11,13)
x+1
y*2
c(length(x),length(y))
x+y
sum(x>5)
sum(x[x>5|x>3])
y[y>7]

# Ej 8
### Compute the mean without using the mean(x) function
x <- c(1,3,5,7,9)
mean <- sum(x)/length(x)
mean
mean(x)
### Compute the variance without using the variance(x) function
var_x <- sum((x - mean)^2) / (length(x) - 1)
var_x
var(x)

# Ej 9
invoices <- c(23,33,25,45,10,28,39,27,15,38,34,29)
### Total we have spent during the year
total_invoices <- sum(invoices)
total_invoices
### Minimum invoice
min_invoice <- min(invoices)
min_invoices
### Maximum invoice
max_invoice <- max(invoices)
max_invoice
### Which is the month name of the maximum invoice?
month_name <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov","Dec")
max_month <- month_name[which(invoices == max_invoice)]
max_month
### In what months have we spent more than average?
expensive_month <- month_name[which(invoices > mean(invoices))]
expensive_month
### Percentage of the months that we have spent more than the mean
percentage_expensive_month <- (100*length(expensive_month))/length(invoices)
cat(percentage_expensive_month,"%\n")

# Ej 10
ans <- read.table(file="anscombe.txt", sep=";", dec = ",", header=F, col.names=c("x1","y1","x2","y2","x3","y3","x4","y4"))
write.table(ans, file="ans.txt", sep=";", dec=".")

# Ej 11
### Create a table just with the columns x1, y1, x3, y3
write.table(ans[, c("x1", "y1", "x3", "y3")], file="ans13.txt", sep=";", dec=".")
### Plot (x1,y1) (x2,y2) (x3,y3) (x4,y4)
par(mfrow=c(2, 2))
plot(ans$x1, ans$y1)
plot(ans$x2, ans$y2)
plot(ans$x3, ans$y3)
plot(ans$x4, ans$y4)
par(mfrow=c(1, 1))

# Ej 12
### N(10,2)
## a) Plot its cumulative probability density function
rango <- seq(4, 16, 0.1)
func_dens_prob_acum <- pnorm(rango, mean=10, sd=2)
plot(x=rango, y=func_dens_prob_acum, type="l", lwd=2)
abline(h=0.5, v=10, lty=2)
### A is higher than 90% of the population
y_a <- 0.9
abline(h=y_a, col="red", lty=3)
x_a <- qnorm(y_a, mean=10, sd=2)
abline(v=x_a, col="red", lty=3)
text(x=x_a, y=0, labels="A", pos=4, col="red")
### Between A and B there is 60% of the population
y_b <- y_a - 0.6
abline(h=y_b, col="blue", lty=3)
x_b <- qnorm(y_b, mean=10, sd=2)
abline(v=x_b, col="blue", lty=3)
text(x=x_b, y=0, labels="B", pos=4, col="blue")
### C is lower than 80% of the population
y_c <- 1 - 0.8
abline(h=y_c, col="green", lty=3)
x_c <- qnorm(y_c, mean=10, sd=2)
abline(v=x_c, col="green", lty=3)
text(x=x_c, y=0, labels="C", pos=4, col="green")
### Between C and B there is 30% of the population
y_d <- y_c + 0.3
abline(h=y_d, col="purple", lty=3)
x_d <- qnorm(y_d, mean=10, sd=2)
abline(v=x_d, col="purple", lty=3)
text(x=x_d, y=0, labels="D", pos=4, col="purple")

## b) Percentages of the population on the left and right of each point
(1 - pnorm(x_a, mean=10, sd=2)) * 100
pnorm(x_a, mean=10, sd=2) * 100
(1 - pnorm(x_b, mean=10, sd=2)) * 100
pnorm(x_b, mean=10, sd=2) * 100
(1 - pnorm(x_c, mean=10, sd=2)) * 100
pnorm(x_c, mean=10, sd=2) * 100
(1 - pnorm(x_d, mean=10, sd=2)) * 100
pnorm(x_d, mean=10, sd=2) * 100

# Ej 13
### N(6,2)
## a) Determine an interval where probability density is the highest 
##      and contains the 70% of the population
y_a <- 0.5 - 0.7 / 2
x_a <- qnorm(y_a, mean=6, sd=2)
x_a
y_b <- 0.5 + 0.7 / 2
x_b <- qnorm(y_b, mean=6, sd=2)
x_b
## b) If the value is chosen randomly, calculate the probability that it's in the 
##      interval (6,7) and in the interval (9,10)
pnorm(7, mean=6, sd=2) - pnorm(6, mean=6, sd=2)
pnorm(10, mean=6, sd=2) - pnorm(9, mean=6, sd=2)
## c) Calculate the probability that the random value is greater than 8 and lower than 4
1 - pnorm(8, mean=6, sd=2)
pnorm(4, mean=6, sd=2)
#### As the distribution is symmetrical, both values are equal because their distance to the mean is the same 
## d) If a sample is bigger or equal to 95%, Which are its possible values?
##      And if it's lower or equal than 30%?
value_95 <- qnorm(0.95, mean=6, sd=2)
cat("[",value_95,", infinity )\n")
value_30 <- qnorm((1 - 0.3), mean=6, sd=2)
cat("( -infinity ,",value_30,"]\n")
