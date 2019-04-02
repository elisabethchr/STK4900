hers.sample=read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/v17/hers.sample.txt",header=T)
hers.sample

plot(hers.sample$age,hers.sample$sbp, xlab="Age", ylab="SBP")

hers.fit.b=lm(sbp~age,data=hers.sample)

summary(hers.fit.b)

abline(hers.fit.b)

cor.test(hers.sample$age,hers.sample$sbp)

#The null hypothesis is that age does not affect the systolic blood pressure, however the
#regression line has a slope of 0.44,m meaning it does have some effect but not a significant
#one, and the Pearson correlation coefficitent is 0.14.

#For someone at the age of 0 years, their blood pressure must be about 105.713

plot(hers.sample$age-67,hers.sample$sbp, xlab="Age", ylab="SBP")
hers.fit.c=lm(sbp~I(age-67),data=hers.sample)
abline(hers.fit.c)

summary(hers.fit.c)

#The blood pressure of women at the age of 67 is 135.2284

plot(hers.sample$age/10,hers.sample$sbp, xlab="Age", ylab="SBP")
hers.fit.d=lm(sbp~I(age/10),data=hers.sample)
abline(hers.fit.d)

summary(hers.fit.d)

#Within 10 years the blood pressure will rise by 4 from 105.713 to ~109.713
