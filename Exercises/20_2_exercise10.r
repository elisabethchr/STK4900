hers=read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/data/hers.txt",sep="\t",header=T,na.strings=".")

hers.no=hers[hers$diabetes==0, ]

summary(hers.no$glucose[hers.no$exercise==0])

summary(hers.no$glucose[hers.no$exercise==1])

boxplot(hers.no$glucose~hers.no$exercise)

#The glucose levels for women who exercise at least three times a week is a bit lower than those who don't, with a mean
#level of 95.67, compared to those who exercise less than three times a week with a mean level of 97.36 Those who work 
#out at least three times a week also has less variance in the glucose levels compared to te women who work out less than
#three times a week

t.test(glucose~exercise, var.equal=T,data=hers.no)

#There is a 95 percentile certainty that the women who work out at least three times a week has a glucose level between
#0.835 and 2.551 lower (??) than those who don't

#linear regression
fit.c=lm(glucose~exercise,data=hers.no)
summary(fit.c)

#The intercept of 97.3610 corresponds to the mean glucose level of those who work out less than three times a week, which
#is what we expect, and shown above in th summary for exercise==0.
#On average, the women who exercise at least three times a week has a mean of -1.6928 lower glucose level, than the women
#who work out less than three times a week. This level is also the mean of the outermost parts of the confidence interval.


fit.d=lm(glucose~exercise+age+BMI,data=hers.no)
plot(hers.no$BMI, hers.no$glucose)
summary(fit.d)
abline(fit.d)

#Once the extra factors are added the mean glucose levels drop due to BMI and age having an effect on the levels as well.
#According to the p-value, the BMI has the greatest significance, while the age has the least effect on the glucose levels.
#The amount of exercise also has a significance 3.5% while compared to when exercise was the only factor included



