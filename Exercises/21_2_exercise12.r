gun=read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/v17/gun.dat", col.names=c("method","phys","team","rounds"))
gun               

#defining covariates as factors:
fmethod = factor(gun$method); fphysique = factor(gun$phys); fteam = factor(gun$team); frounds = factor(gun$rounds)
fmethod

cor(gun)

#Number of rounds is well correlated with method, but not as well with physique, and barely with the team

#Three-way analysis of variance --> Interaction
fit = lm(frounds~fmethod*fphysique*fteam, data=gun)
anova(fit)

summary(gun)
