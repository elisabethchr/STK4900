pef = c(494, 395, 516, 434, 476, 413, 442, 433)
minipef = c(512, 430, 520, 428, 500, 364, 380, 445)

plot(pef, minipef, pch=19, asp=1, main="pef")
cor(pef, minipef)

#We can see from the plot the linear relation between pef and minipef is fair. r~0.8
#The actual Pearson correlation coefficient is r = 0.8154886

cor.test(pef, minipef)

#The CI-interval for the Pearson interval is (0.2605298 0.9653948), where this is the most
#likely interval to find the coefficient.

fit = lm(minipef~pef)
plot(pef, minipef)
abline(fit)
summary(fit)

slope = 1.1642
ratio = sd(pef)/sd(minipef)

slope*ratio

#The two values are similar
