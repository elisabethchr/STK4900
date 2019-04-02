hers=read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/v17/hers.txt",sep="\t",header=T,na.strings=".")
hers

#Find change in LDL over the first year, denoted LDLch
hers$LDLch=hers$LDL1 - hers$LDL
hers$LDLch

hers$cLDL=hers$LDL-145 
hers$cLDL

plot(hers$cLDL, hers$LDL1, pch=18)

#Fit a linear model with the change in LDL as the response and hormone therapy (HT) and baseline LDL (not centered)
#as covariates:

fit.a=lm(LDLch~HT+LDL, data=hers)
summary(fit.a)

#The change over one year decreases, where both the hormone therapy and the baseline LDL has a big significance in
#the effect of the change in LDL.
#The hormone therapy decreases the LDL over one year by 145mg/dL, while the baseline decreases the LDL by 0.388mg/dL.

#We then fit a model with HT and centered LDL at baseline as covariates
fit.b=lm(LDLch~HT+cLDL, data=hers)
summary(fit.b)

#If we are centered at the average when we begin the treatment, then LDL will decrease by 4.89, when not including the7
#hormone therapy


#Model with interaction
fit.c=lm(LDLch~HT+cLDL+HT:cLDL, data=hers)
summary(fit.c)

#The interaction term is significant with a p-value of about 1.29%. In this case, it corresponds to the fact that the
#hormone therapy and the centered LDL baseline is dependent on each other, however it is not as significant as the
#factors of the hormone therapy and centered LDL baseline alone.


#We have two levels, since all participants in the experiment will have a baseline (cLDL = 1), but we can choose to
#include hormone therpy as a factor or not
hers$LDLch=1*(hers$HT==0&hers$cLDL==1)+2*(hers$HT==1&hers$cLDL==1)
hers$LDLch=factor(hers$LDLch)

ht.fit.b=lm(LDL1~LDLch, data=hers)
summary(ht.fit.b)



