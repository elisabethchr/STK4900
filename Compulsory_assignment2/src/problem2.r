#Problem 2:
library(latex2exp)
library(car)
library(gam)
library(ggplot2)
library(plyr)

#Set working directory
setwd('C:\\Users\\Elisabeth\\Documents\\Courses\\STK4900\\Compulsory_assignment2')

olympic <- read.table("olympic.txt", sep="\t", header = TRUE)

olympic
country = olympic$Country
medals2000 = olympic$Total2000
medals1996 = olympic$Total1996
log.pop = olympic$Log.population
log.ath = olympic$Log.athletes
GDP = olympic$GDP.per.cap


#Function for Wald test statistic with 95% confidence interval
Waldtest=function(tab)
{
  regtab=summary(tab)$coef
  coefs = regtab[,1]
  se = regtab[,2]
  lower=coefs - 1.96*se
  upper=coefs + 1.96*se
  z = coefs/se
  cbind(coefs, z, lower, upper)
}

#Function for calculating the means, variances and coefficients of dispersion (CD)
poissontab=function(counts,x=(0:(length(counts)-1)))
{
  n=sum(counts)
  meanx=sum(x*counts)/n
  varx=sum((x-meanx)^2*counts)/(n-1)
  CD=varx/meanx
  df=length(counts)-2
  px=c(dpois(0:df,meanx),1-ppois(df,meanx))
  EX=n*px
  tab=cbind(x,counts,round(EX,2),round(counts-EX,2),round((counts-EX)^2/EX,2))
  X2=sum((counts-EX)^2/EX)
  pval=1-pchisq(X2,df)
  print(paste("Mean=",round(meanx,4)))
  print(paste("Var= ",round(varx,4)))
  print(paste("CD= ",round(CD,4)))
  tab=as.data.frame(tab)
  names(tab)=c("x","Obs","Exp","O-E","(O-E)^2/E")
  print(tab)
  print(paste("Pearson X2 =",round(X2,2)))
  print(paste("p-value��� =",round(pval,4)))
}


#a)
#For a Poisson distribution we know that the expected value and the variance are equal. One method of checking this is to
#look for overdispersion. I.e. if mean(y) and variance s^2 differ (which they shouldn't) too much then CD = s^2/mean(y)>1.
#If CD is greater than 1, it is a sign of overdispersion,i.e. when there is greater variablility in a data set than would
#be expected based on a given statistical model.

#Check if Poisson distributed
summary(medals2000)
table(medals2000)

summary(medals1996)
table(medals1996)

pdf(file = 'C:\\Users\\Elisabeth\\Documents\\Courses\\STK4900\\Compulsory_assignment2\\data\\problem2\\poissondist.pdf')
par(mfrow=c(2,1))
plot(table(medals1996), pch=1, col="blue", xlab="No. of medals", ylab="No. of countries", main=TeX("Olympics 1996"))
plot(table(medals2000), pch=1,col="red", xlab="No. of medals", ylab="No. of countries", main=TeX("Olympics 2000"))
dev.off()

#Checking mean, variance and CD
m = mean(medals2000)
s2 = var(medals2000)
s2/m

m = mean(medals1996)
s2 = var(medals1996)
s2/m


#b) Poisson regression model
fit.2000 = glm(medals2000~offset(log.ath) + log.pop + GDP, data=olympic, family=poisson)
fit.2000
summary(fit.2000)

fit.1996 = glm(medals1996~offset(log.ath) + log.pop + GDP, data=olympic, family=poisson)
fit.1996
summary(fit.1996)

#Wald test
Waldtest(fit.2000)
Waldtest(fit.1996)

#Simplifying model
fit.null = glm(medals2000~offset(log.ath), data=olympic, family=poisson)
fit.pop = glm(medals2000~offset(log.ath) + log.pop, data=olympic, family=poisson)
fit.pop.GDP = glm(medals2000~offset(log.ath) + log.pop + GDP, data=olympic, family=poisson)
anova(fit.null, fit.pop, fit.pop.GDP, test="Chisq")
#Model 2 is best with a p-value of p=1.401e-15

#Simplifying model by interactions
fit.interaction1 = glm(medals2000~offset(log.ath) + log.pop + GDP + log.pop:log.ath, data=olympic, family=poisson)
fit.interaction2 = glm(medals2000~offset(log.ath) + log.pop + GDP + log.pop:log.ath + log.pop:GDP, 
                       data=olympic, family=poisson)
anova(fit.pop, fit.interaction1, fit.interaction2, test="Chisq")






