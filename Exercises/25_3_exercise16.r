n=c(935, 927)
y=c(230, 208)
#create empty list
vec <- vector()
se = c(vec, length(y))
se2 = c(vec, length(y))
p = c(vec, length(y))

#a) Confidence interval

for(i in 1:length(y))
{
  p[i]=y[i]/n[i]
  se[i]=p[i]*(1-p[i])/n[i]
  se2[i] = se[i]^2
}

se = sqrt(sum(se2))
change = p[2] - p[1]
margin=1.96*se
lower=change-margin
upper=change+margin
cbind(change,margin,lower,upper)

#we see that the support for the party Hoyre is on average 22.4% with a margin of error of 3.8%.


#b) Null hypothesis:
p1 = c(y[1]/n[1], y[2]/n[2])
p = (y[1]+y[2])/(n[1]+n[2])
se0=sqrt(p*(1-p)/n[1]+p*(1-p)/n[2])
z=(p1[1]-p1[2])/se0
pval=2*(1-pnorm(abs(z)))
cbind(z,pval)

#The test-statistic for the null hypothesis is z = 1.099, with a p-value of p = 0.27. I.e. we cannot reject the
#null hypothesis due to the test statistic not being large enough, and that there is no real significance between the
#times that the voter's poll was performed.


#c) Comparing proportions

hoyre=matrix(c(y[1],y[2],n[1]-y[1],n[2]-y[2]),nrow=2)   # gives the data for Hoyre in a 2x2 table
prop.test(hoyre,correct=F)

#We see that the results for proportion 2 from population 2 correpsonds to the margin as obtained in task a) and that
#the p-value is the same as that obtained in task b (pval = 0.2717).

#The relation between the test-statistics z and chi-squared (X-squared) is that the chi-squared can be used when extending
#the test statistic to contingency tables of higher order.


#d) Senterpartiet estimates
n=c(935, 927)
y=c(230, 208)
vec <- vector()
se = c(vec, length(y))
p = c(vec, length(y))
se2 = c(vec, length(y))

for(i in 1:length(y))
{
  p[i]=y[i]/n[i]
  se[i]=p[i]*(1-p[i])/n[i]
  se2[i] = se[i]^2
}

se = sqrt(sum(se2))
change = p[2] - p[1]
margin=1.96*se
lower=change-margin
upper=change+margin
cbind(change,margin,lower,upper)

#The change in support for Sp has decreased by about 2.2% with a margin of error of 0.005%.


#Null hypothesis Sp:
p1 = c(y[1]/n[1], y[2]/n[2])
p = (y[1]+y[2])/(n[1]+n[2])
se0=sqrt(p*(1-p)/n[1]+p*(1-p)/n[2])
z=(p1[1]-p1[2])/se0
pval=2*(1-pnorm(abs(z)))
cbind(z,pval)

#When calculating the null-hypothesis we see that the p-value is not significant and that the test-statistic is not
#large enough to reject H0. Thus, there is no significant correlation between the two polls.







