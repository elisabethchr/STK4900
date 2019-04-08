#Problem 1:
library(latex2exp)
library(car)

#Set working directory
setwd('C:\\Users\\Elisabeth\\Documents\\Courses\\STK4900\\Compulsory_assignment2')

crabs <- read.table("crabs.txt", header = TRUE)
crabs

y = crabs$y
width = crabs$width
weight = crabs$weight
color = crabs$color


#a) Logistic regression model
#Since we are considering a binary variable (y = 0, 1), instead of a numerical, we can consider a logistic regression 
#model for studying the probability of presence of satellites.

fit.width = glm(y~width,data=crabs,family=binomial)
print(fit.width)
summary(fit.width)



#b) Odds ratio
OR = exp(coef(fit.width))
conf = confint(fit.width)   #95% confidence interval

OR.width = exp(cbind(OR, conf))

