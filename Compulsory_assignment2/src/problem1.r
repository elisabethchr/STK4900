#Problem 1:
library(latex2exp)
library(car)
library(gam)

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
y_factor = factor(y)

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

#Plot of regression model
fit = glm(y ~ width, data=mtcars, family=binomial)
newdat <- data.frame(width=seq(min(crabs$width), max(crabs$width),len=1000))
newdat$y = predict(fit, newdata=newdat, type="response")

pdf(file = 'C:\\Users\\Elisabeth\\Documents\\Courses\\STK4900\\Compulsory_assignment2\\data\\problem1\\logreg_width.pdf')
plot(y~width, data=crabs, col="red4", main="Logistic regression")
lines(y ~ width, newdat, col="blue3", lwd=2)
legend(21, 0.85, legend=c("Binary data", "Fit"),
       col=c("red4", "blue3"), lty=c(NA, 1), pch=c(1,NA), cex=0.8)
dev.off()


#b) Odds ratio
expcoef(fit.width)


#c) Explanatory variables weight, color, spine
fit.weight = glm(y~weight,data=crabs,family=binomial)
print(fit.weight)
#summary(fit.weight)

fit.color = glm(y~color_group,data=crabs,family=binomial)
print(fit.color)
#summary(fit.color)
summary(y[color_group==4])
plot(color_group, y)

fit.spine = glm(y~spine_group,data=crabs,family=binomial)
print(fit.spine)
summary(y[spine_group==3])
#summary(fit.spine)

fit.all = glm(y~.,data=crabs,family=binomial)
summary(fit.all)
