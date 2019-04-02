library(car)

data_v = read.table("https://www.uio.no/studier/emner/matnat/math/STK4900/data/exer5.dat", col.names = c("salt", "saltprev", "trend", "discharge"))
data_v

summary(data_v)
cor(data_v)

fy = factor(data_v$salt); fx1 = factor(data_v$saltprev); fx2 = factor(data_v$trend); fx3 = factor(data_v$discharge)

plot(data_v)

#From the scatter plots (in the symmetric matrix) we see that the salt and sal salinity from two weeks earlier is highly
#correlated with one another, as well as the river discharge being (negatively) correlated with the salt salinity.

fit = lm(salt~saltprev + trend + discharge, data = data_v)
summary(fit)
anova(fit)

saltfit = fit$fitted.values
saltres = fit$residuals
saltres

#Fitted values are the values which corresponds to the values on the linear regression line (i.e. predicted values)
#when drawing a parallell from the point estimate/observed value to the regression line.

#Residuals is the difference between the observed value and the predicted value (i.e. fitted value).

x1 = saltprev

plot(saltres)

#Checking normality:
hist(saltres)

#We see that the residuals act as a sample from the normal distribution with the mean at zero. So normality is 
#conserved.

boxplot(saltres)
summary(saltres)

#We also see that the residuals has a mean at 0, which corresponds to the fact that the residuals follow a normal
#distribution

qqnorm(saltres); qqline(saltres)

#Checking homoscedasticity
plot(saltfit,saltres, xlab = "Residuals", ylab = "Fitted values")
plot(fit, 1)

summary(fit)

#From the residuals vs fitted values plot, we see that the the fitted regression is fairly constant, thus the variance
#is constant.

#Checking linearity
crPlots(fit, terms=~.)



