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
spine = crabs$spine

color_group = factor(color)
spine_group = factor(spine)

#function for computing odds ratio and relative risk with a 95% CI
expcoef=function(glmobj)
{
  regtab=summary(glmobj)$coef
  expcoef=exp(regtab[,1])
  coefs = regtab[,1]
  lower=expcoef*exp(-1.96*regtab[,2])
  upper=expcoef*exp(1.96*regtab[,2])
  cbind(expcoef, RR, lower,upper)
}


#a) Logistic regression model
fit.width = glm(y~width,data=crabs,family=binomial)
print(fit.width)
summary(fit.width)


#b) Odds ratio
expcoef(fit.width)

#c) Explanatory variables weight, color, spine
fit.weight = glm(y~weight,data=crabs,family=binomial)
print(fit.weight)
#summary(fit.weight)

fit.color = glm(y~color_group,data=crabs,family=binomial)
print(fit.color)
#summary(fit.color)

fit.spine = glm(y~spine_group,data=crabs,family=binomial)
print(fit.spine)
#summary(fit.spine)

