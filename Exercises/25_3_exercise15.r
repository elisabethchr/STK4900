n=935
y=c(309, 122, 80)
parties = c("Ap", "Frp", "Sp")

p=y/n
se=sqrt(p*(1-p)/n)
margin=1.96*se
lower=p-margin
upper=p+margin
cbind(p,margin,lower,upper)

#a) We see that we have a 95% confidence interval of 33% with 3% as a margin of error for the party Ap.
#b)The esitmate for Frp's support is 13% with a margin of error of 2%,
#while the estimate for Sp's support is 8% with a margin of error of 1.7%.
