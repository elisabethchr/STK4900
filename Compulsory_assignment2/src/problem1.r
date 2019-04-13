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
fitgam.width = gam(y~s(width), data=crabs, family=binomial)
plot(fitgam.width, se=T)


#c) Explanatory variables weight, color, spine
fit.weight = glm(y~weight,data=crabs,family=binomial)
print(fit.weight)
summary(fit.weight)

fit.color = glm(y~color_group,data=crabs,family=binomial)
fit.color1 = glm(y~color,data=crabs,family=binomial)

print(fit.color1)
summary(fit.color1)
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

fit.null.weight = glm(y~weight,data=crabs,family=binomial)
fit.null.width = glm(y~width, data=crabs,family=binomial)
fit.weight.width = glm(y~weight+width, data=crabs,family=binomial)
fit.weight.col = glm(y~weight + color_group, data=crabs,family=binomial)
fit.weight.col.width = glm(y~weight + color_group + width, data=crabs,family=binomial)
fit.weight.col.width.spine = glm(y~weight + color_group + spine_group + width, data=crabs,family=binomial)
anova(fit.null.weight, fit.weight.width, fit.weight.col.width, fit.weight.col.width.spine, test="Chisq")
#fit.weight.width is significant with a p-value of p = 0.09164. fit.weight.col, compared to fit.weight.width, is the most
#significant model with a p-value of p = 0.08280.
#End up with model 3 (fit.weight.col)


#Comparison of two different models with width vs weight:
fit.null.col = glm(y~color_group, data=crabs,family=binomial)
fit.width.col = glm(y~width+color_group, data=crabs,family=binomial)
anova(fit.null.col, fit.weight.col, fit.weight.col.width, test="Chisq")
#End up with model 2, with a p-value of p = 7.041e-07.
anova(fit.null.col, fit.width.col, fit.weight.col.width, test="Chisq")
#End up with model 2 with a p-value of p = 1.237e-06.

#In total, fit.width.col is a better fit than fit.weight.col, where we get a p-value of p = 7.041e-07 for fit.width.col 
#compared to fit.weight.col with p = 1.735e-06. Both models are a better fit than when including all three predictors
#in the same model (fit.weight.col.width).


#Residuals and fitted values from best model:
fit.width.col.res = fit.width.col$residuals
fit.width.col.fitval = fit.width.col$fitted.values

newdat <- data.frame(width=seq(min(fit.width.col.fitval), max(fit.width.col.fitval),len=1000))
newdat$y = predict(fit, newdata=newdat, type="response")

plot(fit.width.col.fitval, fit.width.col.res, col="red", main="Resdiuals vs. fitted values", xlab="Fitted", 
     ylab="Residuals")
lines(lowess(fit.width.col.fitval,fit.width.col.res),col="blue",lwd=2)
abline(h=0,lty=2,col="grey")


#e) Interactions
#Using the best fit from task d, where only color_group had a significance on the model and check if interactions with
#other predictors can improve the model using likelihood ratio test:
fit.col.interaction1 = glm(y~color_group + width, data=crabs,family=binomial)
fit.col.interaction2 = glm(y~color_group + width + weight:color_group:width + width:weight, data=crabs,family=binomial)
fit.col.interaction3 = glm(y~color_group + width + width:weight + weight:color_group:spine, data=crabs,family=binomial)
fit.col.interaction4 = glm(y~color_group + width + width:weight + weight:color_group + spine:width:weight, data=crabs,family=binomial)

#fit.col.interaction3 = glm(y~color_group + width + color_group:width + color_group:weight + color_group:spine_group, 
#                           data=crabs, family=binomial)
#fit.col.interaction4 = glm(y~color_group + color_group:width + color_group:weight, data=crabs,family=binomial)
fit.col.interaction5 = glm(y~color_group + color_group:width + color_group:spine_group, data=crabs,family=binomial)
anova(fit.col.interaction1, fit.col.interaction2, fit.col.interaction3, fit.col.interaction4, test="Chisq")
#None of these interactions are signifacnt enough to be taken into consideration. Thus, we are still left width
#fit.width.col as the best fit.

