advertising <- read.csv('Advertising.csv')
#Advertising
#fix(Advertising)
#typeof(Advertising)
plot(advertising, main="Correlation")
attach(advertising) # for not writing dataframe$feature, just feature

linearMod <- lm(Sales~TV)
summary (linearMod)

# Plotting conclussions
par(mfrow=c(4, 2))  # divide graph area in 2 columns

boxplot(Sales, main="Sales")  # box plot for 'speed'
boxplot(TV, main="TV")  # box plot for 'distance'

hist(Sales, main="Sales")
hist(TV, main="TV")

plot(TV,Sales, main='Scatter Plot')
plot(TV,Sales, main='Linear Regression')
abline(linearMod,lwd=3,col=2)

res <- resid(linearMod)
plot(TV, res, ylab="Residuals", xlab="TV", main="Residuals")

# histogram blocks of 10 steps
res_mean = by(abs(res), ceiling(1:200 / 10), mean)
barplot(res_mean)

