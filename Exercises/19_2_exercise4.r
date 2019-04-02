solvents=read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/data/solvents.txt",header=T)
solvents

boxplot(rate~type,data=solvents)

#Type 3 (esters) has the lowest mean sorption rate, however it also has the lowest standard deviation, 
#while type 2 (chloroalkanes) has the highest mean sorption rate with the highest standard deviation

solvents$type = factor(solvents$type)
aov.solvents=aov(rate~type,data=solvents)
summary(aov.solvents)

#Since the p-value is small, then the null hpothesis can be rejected and the different types have different
#sorption rates as obesrved from the boxplots



