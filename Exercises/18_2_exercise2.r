cont=c(0.228, 0.207, 0.234, 0.220, 0.217, 0.228, 0.209, 0.221, 0.204, 0.220, 0.203, 0.219, 0.218, 0.245, 0.210)

treat=c(0.250, 0.237, 0.217, 0.206, 0.247, 0.228, 0.245, 0.232, 0.267, 0.261, 0.221, 0.219, 0.232, 0.209, 0.255)

m1 = mean(cont)  #0.2188667
m2 = mean(treat) #0.2350667
s1 = sd(cont)    #0.01158735
s2 = sd(treat)   #0.01877105

n1 = length(cont)
n2 = length(treat)
dof = n1+n2-2
dof

summary(cont)
summary(treat)

t.test(treat, cont, var.equal=T)

#create matrix, can also use data.frame
mat <- cbind(cont, treat)
mat

#pooled standard deviation (possible to use pooled.sd)
sp = sqrt((n1-1)/dof*s1^2 + (n2-1)/dof*s2^2)
sp

#confidence
c = 2.048

#standard deviation
se = sp*sqrt(1/n1 + 1/n2)
se

#confidence interval
CI1 = m2 - m1 + c*se
CI2 = m2-m1-c*se
CI1; CI2

#t-statistic
tstat = (m2-m1)/se
tstat

#P-value
p = 1 - pt(tstat, dof)
p*2
