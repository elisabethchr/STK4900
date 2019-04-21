#Problem 3:
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
fit.treat=survfit(Surv(time,status==1)~factor(treat), data=cirrhosis)
pdf(file='data\\problem3\\KM_fittreat.pdf')
par(mfrow=c(1,1))
plot(fit.treat, lty=1:2, mark.time=F, col=c('red', 'black'), xlab = 'Time [days]', ylab='Survival', main='Treatment')
legend(3500, 0.9, legend=c('Prednisone', 'Placebo'), lty=1:2, col=c('red', 'black'))
dev.off()
print(fit.treat)

#gender:
fit.sex=survfit(Surv(time,status==1)~factor(sex), data=cirrhosis)
pdf(file='data\\problem3\\KM_fitsex.pdf')
par(mfrow=c(1,1))
plot(fit.sex, lty=1:2, mark.time=F, col=c('red', 'blue'), xlab = 'Time [days]', ylab='Survival', main='Gender')
legend(3500, 0.9, legend=c('Female', 'Male'), lty=1:2, col=c('red', 'blue'))
dev.off()
print(fit.sex)

#ascites:
fit.asc=survfit(Surv(time,status==1)~factor(asc), data=cirrhosis)
pdf(file='data\\problem3\\KM_fitascites.pdf')
par(mfrow=c(1,1))
plot(fit.asc, lty=1:3, mark.time=F, col=c('red', 'blue', 'black'), xlab = 'Time [days]', ylab='Survival', main='Ascites')
legend(3500, 0.9, legend=c('None', 'Slight', 'Marked'), lty=1:3, col=c('red', 'blue', 'black'))
dev.off()
print(fit.asc)

#grouped age
fit.agegr=survfit(Surv(time,status==1)~factor(agegr), data=cirrhosis)
pdf(file='data\\problem3\\KM_fitagegr.pdf')
par(mfrow=c(1,1))
plot(fit.agegr, lty=1:3, mark.time=F, col=c('red', 'blue', 'black'), xlab = 'Time [days]', ylab='Survival', main='Age group')
legend(3500, 0.9, legend=c('<50', '50-65', '>65'), lty=1:3, col=c('red', 'blue', 'black'))
dev.off()
print(fit.agegr)


#b) Logrank test
survdiff(Surv(time,status==1)~factor(treat), data=cirrhosis)  #treat
survdiff(Surv(time,status==1)~factor(sex), data=cirrhosis)    #gender
survdiff(Surv(time,status==1)~factor(asc), data=cirrhosis)    #asc
survdiff(Surv(time,status==1)~factor(agegr), data=cirrhosis)  #agegr


#c) Multiple Cox regression and hazard ratio
#Effects of all ovariants studied simultaneously
fit.all=coxph(Surv(time,status==1)~factor(sex)+factor(treat) + age + factor(asc) ,data=cirrhosis)
print(fit.all)
summary(fit.all)

fit.women=coxph(Surv(time,status==1)~factor(sex==0)+factor(treat) + age + factor(asc) ,data=cirrhosis)
fit.men=coxph(Surv(time,status==1)~factor(sex==1)+factor(treat) + age + factor(asc) ,data=cirrhosis)
summary(fit.women)
summary(fit.men)

HRratio(fit.all)

