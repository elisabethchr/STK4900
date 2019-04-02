sales = c(508.1, 498.4, 568.2, 577.3, 651.7, 657.0, 755.3, 758.9, 787.6, 792.1, 841.4, 831.8,
          854.7, 871.4)
disp = c(0, 0, 1, 1, 2, 2, 4, 4, 5, 5, 6, 6, 7, 7)

length(sales)
length(disp)

plot(disp, sales, pch=19)

fit = lm(sales~disp)
abline(fit)

fit.two = lm(sales~disp+I(disp^2))

x = seq(0, 7, 0.1)
koef = lm(sales~disp + I(disp^2))$coef  #$coef = extract model coefficients
koef

lines(x,koef[1]+koef[2]*x+koef[3]*x^2, lty=2)

#The second order polynomial fit is the regression line which fits the data best, and is thus to be preferred