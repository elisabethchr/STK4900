#Problem 2:
library(latex2exp)
library(car)

setwd('C:\\Users\\Elisabeth\\Documents\\Courses\\STK4900\\Compulsory_assignment1')  #set working directory

datablood <- read.table("blood.txt",sep=",",header=TRUE)   
#sep="\t" => the values in the file are separated by tabs
#header = TRUE => the first row in the file contains the names of each column
datablood

bloodtr = datablood$blodtr; ages = datablood$alder

#Defining covariates as factors, i.e. categorical covariates:
age_group = factor(ages)
age_group
#a)
summary(bloodtr[age_group==1])
summary(bloodtr[age_group==2])
summary(bloodtr[age_group==3])

pdf("boxplot_a.pdf")
boxplot(bloodtr~alder,data=datablood, main="Boxplots of blood pressure")
dev.off()


#b)
#Assumptions of one-way ANOVA
#1. Each sample is taken from a normally distributed population
#2. Each sampe has been drawn independently from the others
#3. The variance of the data if the different groups are the same
#4. The variable, on which the measurements are based on, can be subdivided into separable groups, i.e. in this case
#different age groups
#5. Three or more groups are to be compared. (Two-way ANOVA compares data where each variable should have multiple
#samples)

#one-way ANOVA:
fit.datablood = lm(bloodtr~age_group, data=datablood)
anova(fit.datablood)


#c)
fit = lm(bloodtr~age_group, data=datablood)
summary(fit)
