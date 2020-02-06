#############################################################
########### Linear Regression on Advertising data ###########
#############################################################

# List objects in this session
list=ls()

# remove all (memory clean)
rm(list=ls())

# Set Working directory
setwd("/home/jaime/UPM/MLLB/Machine_Learning_Lab/1_Linear_Regression")

#Read the csv file into Advertising
Advertising=read.csv("Advertising.csv")
fix(Advertising)
plot(Advertising)

# Explore scatter plots
plot(Advertising$TV,Advertising$Sales)
attach(Advertising)
plot(TV,Sales)

par(mfrow=c(2,2))
plot(TV,Sales)
plot(Radio,Sales)
plot(News.paper,Sales)

# Explore scatter plots
pairs(~ TV + Radio + News.paper + Sales, Advertising) 

# More advanced ggplot (2)
# https://deanattali.com/2015/03/29/ggExtra-r-package/

library(ggplot2)
# create a ggplot2 scatterplot
p <- ggplot(Advertising, aes(TV, Sales)) + geom_point() + theme_classic()
# add marginal histograms
ggExtra::ggMarginal(p, type = "histogram")

############# TV ###################
# Simple Linear Regression: Sales ~ TV
lmTV.fit=lm(Sales~TV,data=Advertising)
attach(Advertising)

# Ploting resduals
lmTV.fit=lm(Sales~TV)
lmTV.res=resid(lmTV.fit)
par(mfrow=c(2,2))
plot(lmTV.fit)

par(mfrow=c(1,1))
plot(lmTV.res)
abline(0,0)

# Plotting the Regression Line
par(mfrow=c(1,1))
plot(TV,Sales)
abline(lmTV.fit,lwd=3,col=2)

summary(lmTV.fit)

# Linear model plot
ggplot(Advertising, aes(TV, Sales)) + geom_point() + geom_smooth(method=lm)

######## NEWSPAPER #################
#Simple Linear Regression: Sales ~ news.paper
lmNews.paper.fit=lm(Sales~News.paper)

# Plotting the Regression Line
par(mfrow=c(1,1))
plot(News.paper,Sales)
abline(lmNews.paper.fit,lwd=3,col=2)

summary(lmNews.paper.fit)

######### RADIO ####################
#Simple Linear Regression: Sales ~ Radio
lmRadio.fit=lm(Sales~Radio)

# Plotting the Regression Line
par(mfrow=c(1,1))
plot(Radio,Sales)
abline(lmRadio.fit,lwd=3,col=2)

summary(lmRadio.fit)