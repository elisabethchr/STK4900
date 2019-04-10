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
  cbind(expcoef, lower,upper)
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
summary(fit.weight)

fit.color = glm(y~color_group,data=crabs,family=binomial)
print(fit.color)
summary(fit.color)
summary(y[color_group==4])
#Odds ratio for each group
expcoef(fit.color)

fit.spine = glm(y~spine_group,data=crabs,family=binomial)
print(fit.spine)
summary(y[spine_group==3])
summary(fit.spine)
#Odds ratio for each group
expcoef(fit.spine)


#d) All variables in regression
fit.all = glm(y~ weight + width + color_group + spine_group,data=crabs,family=binomial)
summary(fit.all)
#anova(fit.all)
#We see that all coefficient has an estimate different from zero. To check whether or not a predictor actually has an
#impact on the model we can run a likelihood ratio test, comparing one with the predictor and one without the predictor.


#Likelihood ratio test - run ANOVA and compare deviances when either including or excluding certain predictors

fit.null = glm(y~log(weight))
fit.col = glm(y~log(weight) + color_group)
fit.col.spine = glm(y~log(weight) + color_group + spine_group + width)
fit.col.spine.width = glm(y~log(weight) + color_group + spine_group + width)
fit.col1 = glm(y~color_group)
fit.spine = glm(y~spine_group)
fit.width = glm(y~width)
anova(fit.null, fit.col, fit.col.spine, fit.col.spine.width, fit.col1, fit.spine, fit.width, test="Chisq")
#fit.col is significant with a p-value of p = 0.03969 and fil.col1 is significant with p = 1.41e-05.
#End up with model 5 (fit.col1)


#e) Interactions
#Using the best fit from task d, where only color_group had a significance on the model and check if interactions with
#other predictors can improve the model using likelihood ratio test:
fit.col.interaction1 = glm(y~color_group + color_group:width)
fit.col.interaction2 = glm(y~color_group + color_group:weight)
fit.col.interaction3 = glm(y~color_group + color_group:spine_group)
fit.col.interaction4 = glm(y~color_group + color_group:width + color_group:weight)
fit.col.interaction5 = glm(y~color_group + color_group:width + color_group:spine_group)
anova(fit.col1, fit.col.interaction1, fit.col.interaction2, fit.col.interaction3, fit.col.interaction4,
      fit.col.interaction5, test="Chisq")

#We end up with model 2 as the best model, with an interaction between the color_group and width of the horseshoe crabs,
#with a p-value of p = 4.267e-06


