library(survival)

setwd('C:\\Users\\Elisabeth\\Documents\\Courses\\STK4900\\Exercises')  #set working directory

gehan <- read.table("gehan.txt", header=TRUE)   
gehan

time = gehan$time
cens = gehan$cens
treat = gehan$treat


fit.1=survfit(Surv(time,cens)~treat, conf.type="none", data=gehan)
summary(fit.1)
print(fit.1)

#Plot of Kaplan-Meier estimate
plot(survpred, lty=1:2)

#Kaplan-Meier estimate with confidence limits
fit.2=survfit(Surv(time,cens)~treat, conf.type="plain",data=gehan)
summary(fit.2)
plot(fit.2, conf.int=T, lty=1:2, col=c('black', 'blue'))
print(fit.2)

#There is a clear difference between the two groups, where those who were treated with the drug have a higher survival
#rate than those who were not treated with the drug.
#We see that the median lifetime for those who were not treated with the drug is approximately 8 years, while those who
#were treated with the drug have a median lifetime of about 23 years.
#We can also see that the confidence intervals for the two groups overlap each other for a small period of time
#signifying that the CI's in this period is uncertain.


#logrank-test
survdiff(Surv(time, cens)~treat)

#We see that there is a significant difference between the two groups and we can thus reject the null-hypothesis H0.


#Cox regression model
fit.4=coxph(Surv(time,cens)~factor(treat), data=gehan)
summary(fit.4)

#From the summary we see that the P-value for the group which was treated with the drug is less that 5%. Thus the
#treatment from the drug is highy significant on the survival rate of the subjects. This can also be seen from the
#hazard ratio test where those who were treated with the drug is likely to live 20% longer of the lifetime 
#than those who were not treated with the drug.




