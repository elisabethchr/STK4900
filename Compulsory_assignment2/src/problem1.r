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

pdf(file = 'data\\problem1\\logreg_width.pdf')
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
summary(fit.color)
summary(y[color_group==4])

fit.spine = glm(y~spine,data=crabs,family=binomial)
print(fit.spine)
summary(y[spine_group==3])
summary(fit.spine)

#Odds ratio for each predictor
expcoef(fit.spine)
expcoef(fit.width)
expcoef(fit.color)

#d) All variables in regression
fit.all = glm(y~ weight + width + color_group + spine_group,data=crabs,family=binomial)
summary(fit.all)


#Likelihood ratio test - run ANOVA and compare deviances when either including or excluding certain predictors

fit.null.weight = glm(y~weight,data=crabs,family=binomial)
fit.null.col = glm(y~color_group, data=crabs,family=binomial)
fit.width.col = glm(y~width + color_group, data=crabs,family=binomial)
fit.width.col.weight = glm(y~width+color_group +spine_group, data=crabs,family=binomial)
fit.width.col.weight.spine = glm(y~width + color_group + weight + spine_group, data=crabs,family=binomial)
anova(fit.null.col, fit.width.col, fit.width.col.weight, fit.width.col.weight.spine, test="Chisq")


#Including 2'nd order terms:
fit.width.col = glm(y~width + color_group, data=crabs,family=binomial)
fit.width2.col = glm(y~width + color_group +I(width^2), data=crabs,family=binomial)
anova(fit.width.col, fit.width2.col, test="Chisq")
#Still end up with the original model as above due to the p-values being too high

#Comparison of two different models with width vs weight:
fit.null.col = glm(y~color_group, data=crabs,family=binomial)
fit.width.col = glm(y~width+color_group, data=crabs,family=binomial)
fit.weight.col = glm(y~width+spine+color_group, data=crabs, family=binomial)
anova(fit.null.col, fit.width.col, fit.weight.col, test="Chisq")
anova(fit.null.col, fit.width.col, fit.weight.col.width, test="Chisq")

summary(fit.width.col)

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
fit.width.col = glm(y~color_group + width, data=crabs,family=binomial)
fit.col.interaction1 = glm(y~color_group + width + weight:width)
fit.col.interaction2 = glm(y~color_group + width + weight:width + width:color_group, data=crabs, family=binomial)
fit.col.interaction3 = glm(y~color_group + width + weight:width + width:color_group + weight:color_group,
                           data=crabs,family=binomial)
fit.col.interaction4 = glm(y~color_group + width + width:weight + width:color_group + weight:color_group + 
                             weight:color_group:spine_group, data=crabs,family=binomial)

anova(fit.width.col, fit.col.interaction1, fit.col.interaction2, fit.col.interaction3, fit.col.interaction4, test="Chisq")
