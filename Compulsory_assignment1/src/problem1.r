#Problem 1:
library(latex2exp)
library(car)

setwd('C:\\Users\\Elisabeth\\Documents\\Courses\\STK4900\\Compulsory_assignment1')  #set working directory

no2data <- read.table("no2.txt",sep="\t",header=TRUE)   
#sep="\t" => the values in the file are separated by tabs
#header = TRUE => the first row in the file contains the names of each column
no2data

summary(no2data)

log.cars = no2data$log.cars
log.no2 = no2data$log.no2
temp = no2data$temp
wind.speed = no2data$wind.speed
hour.of.day = no2data$hour.of.day

#a)
#open pdf-file
plot(log.cars, log.no2, xlab="log(cars per hour)", ylab = TeX("Concentration of NO$_2$"), #$[1\micro gm^{-3}$]") ,
     main =  TeX("Number of cars vs amount of $NO_2$"), col = "red", pch=1)


#b)
fit.no2.cars = lm(log.no2~log.cars)
pdf("cars_NO2.pdf")
abline(fit.no2.cars)

#close pdf-file
dev.off()

legend = "Fit"
legend("topleft", inset = .03, legend, col = c("black"), lty=1, cex=1)

summary(fit.no2.cars)
#checking the correlation between the independent variable log(cars) and log(NO2)
cor.test(log.cars, log.no2)

#c) Checking various plot and residuals:
no2.cars.res = fit.no2.cars$residuals
no2.cars.fitval = fit.no2.cars$fitted.values
summary(no2.cars.res)

#check of constant variance (homoscedasticity)
plot(no2.cars.fitval, no2.cars.res, pch=1)
pdf("no2_cars_const_variance.pdf")
plot(fit.no2.cars, 1)
dev.off()

#check of normality
pdf("no2_cars_hist.pdf")
hist(no2.cars.res, breaks = 20, main = TeX("Histogram of residuals"),xlab = TeX("Resdiuals"), border = "red")
dev.off()

pdf("no2_cars_boxplot.pdf")
boxplot(no2.cars.res, main="Boxplot of residuals", border="blue")
dev.off()

pdf("no2_cars_qq.pdf")
qqnorm(no2.cars.res, col = "red"); qqline(no2.cars.res)
legend = c("Distribution", "Reference")
legend("topleft", inset = .03, legend, col = c("red", "black"), pch = c(1, NA), lty=c(NA, 1), cex=1)
dev.off()

#Check of linearity
crPlots(fit.no2.cars, terms=~log.cars)


#d) Best model fit
fit.no2.temp = lm(log.no2~temp)
no2.temp.res = fit.no2.temp$residuals
no2.temp.fitval = fit.no2.temp$fitted.values

cor.test(log(hour.of.day), log.no2)

fit.no2.windspeed = lm(log.no2~log(wind.speed))

fit.no2.hod = lm(log.no2~hour.of.day)

crPlots(fit.no2.temp, terms=~temp)
crPlots(fit.no2.windspeed, terms=~log(wind.speed))

fit.1 = lm(log.no2~log.cars + wind.speed + hour.of.day + temp)
summary(fit.1)

fit.2 = lm(log.no2~log.cars + log(wind.speed) + hour.of.day + temp)
summary(fit.2)

fit.3 = lm(log.no2~log.cars + log(wind.speed) + log(hour.of.day) + temp)
summary(fit.3)

fit.4 = lm(log.no2~log.cars + wind.speed + log(hour.of.day) + temp)
summary(fit.4)

anova(fit.1)
anova(fit.2)

best_fit_values = fit.2$fitted.values
best_fit_res = fit.2$residuals

#check of constant variance (homoscedasticity)
plot(best_fit_values, best_fit_res, pch=1)
plot(fit.2, 1)

#check of normality
hist(best_fit_res, breaks = 20, main = TeX("Histogram of $NO_2$ - cars residuals"),xlab = TeX("Resdiuals"))
boxplot(best_fit_res, main="Boxplot of residuals")
qqnorm(b, est_fit_res, col = "red"); qqline(best_fit_res)

#Check of linearity
crPlots(fit.2, terms=~log(wind.speed))
fit.sq2 = lm(log.no2~log.cars + log(wind.speed) + I(log(wind.speed)^2) + I(log(wind.speed)^5) + hour.of.day + temp)
crPlots(fit.sq2, terms=~log.cars + I(log(wind.speed)^2) + hour.of.day + temp)
summary(fit.sq2)

best_fit_values_sq = fit.sq2$fitted.values
best_fit_res_sq = fit.sq2$residuals
plot(best_fit_values_sq, best_fit_res_sq, pch=1)
plot(fit.sq2, 1)

#Attempt at better adjusted R^2:
fit.sq3 = lm(log.no2~log.cars + I(log(wind.speed)^2) +
               I(log(wind.speed)^3) + I(log(wind.speed)^4) + I(log(wind.speed)^5) + I(hour.of.day^2) +
               I(hour.of.day^3) + temp)
pdf("best_fit_model.pdf")
crPlots(fit.sq3, terms=~log.cars + I(log(wind.speed)^2) + hour.of.day + temp)
dev.off()
summary(fit.sq3)
plot(fit.sq3)

anova(fit.sq3)
