speed = c(28, -44, 29, 30, 24, 28, 37, 32, 36, 27, 26, 28, 29, 26, 27, 22, 23, 20, 25, 25, 
          36, 23, 31, 32, 24, 27, 33, 16, 24, 29, 36, 21, 28, 26, 27, 27, 32, 25, 28, 24,
          40, 21, 31, 32, 28, 26, 30, 27, 26, 24, 32, 29, 34, -2, 25, 19, 36, 29, 30, 22,
          28, 33, 39, 25, 16, 23)

hist(speed)
plot(ecdf(speed), verticals=T, do.points=F)
boxplot(speed)

#The two outliers correspond to the negative values of the data set. I.e. they are distinct deviations from the data set (?)

summary(speed)

t.test(speed, alternative="greater")

mean(speed)
median(speed)

#The two measures of "location" is given by the mean and the median. The mean tells us the average of all measurements,
#while the median tells us the "middle number" of the data set while in chronological order


sd(speed)
IQR(speed)

#The measures of spread tells us the average deviation of the average of the data set (sd), and the difference in the 
#two medians from the lower half of the data set (Q1) and the upper half of the data set (Q2)

#computing the two-sided confidence interval
dof = length(speed) - 1

no_outliers = speed > 0
speed_no = speed[no_outliers]

mean(speed_no) - qt(0.975, dof)*(sd(speed_no)/sqrt(length(speed_no)))
mean(speed_no) + qt(0.975, dof)*(sd(speed_no)/sqrt(length(speed_no)))

summary(speed)
t.test(speed)

e = 1.60e-19
c = 3.0*10^8
eps = 8.85*10^(-12)
E = 1.60*10^(-7)
Ps = 0.10155*10^9
m0 = 0.511*10^6/c^2

R = sqrt(e^2*c/(6*pi*eps)*1/(m0*c^2)^4*E^4/Ps)
R
sqrt(e^2*c/(6*pi*eps)*(m0*c^2)^(-4)*E^4/Ps)

