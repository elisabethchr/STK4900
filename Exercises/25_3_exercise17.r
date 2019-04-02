wcgs=read.table("http://www.uio.no/studier/emner/matnat/math/STK4900/data/wcgs.txt", sep="\t",header=T,na.strings=".")
wcgs

table(wcgs$smoke,wcgs$chd69)

#a) Relative risk and odds ratio

p1=159/1502
p0=98/1652
RR=p1/p0
OR=(p1/(1-p1))/(p0/(1-p0))
cbind(p1,p0,RR,OR)



#b) Logistic regression with smoke as covariate

fit.smoke=glm(chd69~smoke,data=wcgs,family=binomial)
print(fit.smoke)

#We see that the chances of acquiring colonary heart disease increase by 0.6299 for each unit if they smoke, compared
#to that of a non-smoker


#c) Effect of age

fit.age=glm(chd69~age,data=wcgs,family=binomial)
print(fit.age)

#The chances of acquiring colonary heart disease increase by 0.07442 for each year.
