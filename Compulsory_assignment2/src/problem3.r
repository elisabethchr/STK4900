#Problem 2:
library(latex2exp)
library(car)
library(gam)
library(ggplot2)
library(plyr)
library(survival)

#Set working directory
setwd('C:\\Users\\Elisabeth\\Documents\\Courses\\STK4900\\Compulsory_assignment2')

cirrhosis <- read.table("cirrhosis.txt", sep="\t", header = TRUE)

cirrhosis

status = cirrhosis$status
time = cirrhosis$time
time
treat = cirrhosis$treat
sex = cirrhosis$sex
asc = cirrhosis$asc
age = cirrhosis$age
agegr = cirrhosis$agegr


#Funtion for calculating hazard ratio
HRratio = function(data)
{
  tab = summary(data)$coef
  coefs = tab[,1]
  HR = exp(coefs)
  cbind(coefs, HR)
}

#a) Kaplan-Meier plots
#treatment:
fit.treat=survfit(Surv(time,status==1)~treat, data=cirrhosis)
par(mfrow=c(1,1))
plot(fit.treat, lty=1:2, mark.time=F, col=c('red', 'black'), xlab = 'Time [days]', ylab='Survival')
print(fit.treat)

#sex:
fit.sex=survfit(Surv(time,status==1)~factor(sex), data=cirrhosis)
par(mfrow=c(1,1))
plot(fit.sex, lty=1:2, mark.time=F)
print(fit.sex)

#ascites:
fit.asc=survfit(Surv(time,status==1)~asc, data=cirrhosis)
par(mfrow=c(1,1))
plot(fit.asc, lty=1:3, mark.time=F)
print(fit.asc)

#grouped age
fit.agegr=survfit(Surv(time,status==1)~agegr, data=cirrhosis)
par(mfrow=c(1,1))
plot(fit.agegr, lty=1:3, mark.time=F)
print(fit.agegr)


#b) Logrank test
survdiff(Surv(time,status==1)~treat, data=cirrhosis)  #treat
survdiff(Surv(time,status==1)~sex, data=cirrhosis)    #sex
survdiff(Surv(time,status==1)~asc, data=cirrhosis)    #asc
survdiff(Surv(time,status==1)~agegr, data=cirrhosis)  #agegr


#c) Cox regression and hazard ratio
#Effects of all ovariants studied simultaneously
fit.all=coxph(Surv(time,status==1)~factor(sex)+factor(treat) + age + factor(asc) ,data=cirrhosis)
print(fit.all)
summary(fit.all)

HRratio(fit.all)

