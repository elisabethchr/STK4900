#Read data
insurance = read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/data/exer3_2.dat")
insurance
income = insurance$V1; amount = insurance$V3; ras = insurance$V2

#Different regression statements:
#First order polynomial fit:
fit_income = lm(amount~income)
fit_ras = lm(amount~ras)

#Second order polynomial fit
fit_income.two = lm(amount~income + I(income^2))
fit_ras.two = lm(amount~ras + I(ras^2))

summary(fit_income.two)
summary(fit_income)
summary(fit_ras.two)
summary(fit_ras)

x_income = seq(min(income), max(income), 0.1)
x_ras = seq(min(ras), max(ras), 0.1)

koef_income = lm(amount~income + I(income^2))$coef  #$coef = extract model coefficients
koef_ras = lm(amount~ras + I(ras^2))$coef  #$coef = extract model coefficients
koef_income; koef_ras


#Pearson correlation coefficients:
cor.test(income, amount)
cor.test(ras, amount)


#Plotting:
par(mfrow=c(1,2))
plot(income, amount, xlab = "Income", ylab = "Amount", ylim = c(0, 150),pch=19)
abline(fit_income)
lines(x_income,koef_income[1]+koef_income[2]*x_income+koef_income[3]*x_income^2, lty=2)
plot(ras, amount, xlab = "Risk aversion score", ylab = "Amount", pch=19)
abline(fit_ras)
lines(x_ras,koef_ras[1]+koef_ras[2]*x_ras+koef_ras[3]*x_ras^2, lty=2)
par(mfrow=c(1,1))

#For income vs amount we see that both the first and second order polynomial correspond, i.e. they overlap each other.
#In this plot, the correlation coefficient is also quite high (r = 0.9), meaning that there is a strong correlation
#between the income one has and the amount of insurance carried.
#On the other hand, for the plot of the risk aversion score vs the amount of insurance carried we see that the tat
#the second order polynomial has the best fit due to its R_squared is R^2 = 0.5537, compared to the R_squared of the
#first order polynomial fit which is R^2 = 0.1591


