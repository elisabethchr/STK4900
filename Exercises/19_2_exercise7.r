library(MASS)

n=400

rho=0.9

m=matrix(c(0,0),nrow=2)

S=matrix(c(1,rho,rho,1),nrow=2)

obs=mvrnorm(n,m,S)
obs
x=obs[,1]

y=obs[,2]

cor(x,y)

plot(x,y)

cor.test(x, y)

#Pearson correlation coefficient r = 0.37