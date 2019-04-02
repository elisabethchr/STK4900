library(survival)

setwd('C:\\Users\\Elisabeth\\Documents\\Courses\\STK4900\\Exercises')  #set working directory

melanoma <- read.table("melanoma.txt", header=TRUE)   
melanoma

status = melanoma$status
lifetime = melanoma$lifetime
ulcer = melanoma$ulcer
thickn = melanoma$thickn
sex = melanoma$sex
age = melanoma$age
grthick = melanoma$grthick
logthick = melanoma$logthick


fit.sex=survfit(Surv(lifetime,status==1)~sex, data=melanoma)
plot(fit.sex, lty=1:2, mark.time=F, col=c('black', 'red'))

#Logrank test
survdiff(Surv(lifetime,status==1)~sex, data=melanoma)

#b)
legend1=c('0-1mm', '2-5mm', '5+ mm')
fit.grthick=survfit(Surv(lifetime,status==1)~grthick, data=melanoma)
plot(fit.grthick, lty=1:3, mark.time=F, col=c('black', 'red', 'blue'), legend(0.5, 0.6, legend1, lty=1:3, 
                                                                              col=c('black', 'red', 'blue')))

#c)
plot.new()
fit.ulcer=survfit(Surv(lifetime,status==1)~ulcer, data=melanoma)
plot(fit.ulcer, lty=1:3, mark.time=F, col=c('black', 'red'), legend(0.5, 0.4, c('Present', 'Absent'), lty=1:2, 
                                                                    col=c('black', 'red')))
                                                                  
#d)
coxfit.sex=coxph(Surv(lifetime,status==1)~factor(sex), data=melanoma)
summary(coxfit.sex)

coxfit.grthick=coxph(Surv(lifetime,status==1)~factor(grthick), data=melanoma)
summary(coxfit.grthick)

coxfit.ulcer=coxph(Surv(lifetime,status==1)~factor(ulcer), data=melanoma)
summary(coxfit.ulcer)

coxfit.logthick=coxph(Surv(lifetime,status==1)~factor(logthick), data=melanoma)
summary(coxfit.logthick)







