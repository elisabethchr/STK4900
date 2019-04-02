import numpy as np

beta = [4.650, 0.121, 0.033, -0.134, -0.032, -0.065, 0.073, -0.028, 0.138, 0.105]

sd = [0.90, 0.041, 0.043, 0.046, 0.046, 0.046, 0.043, 0.043, 0.043, 0.047]

n = 29.

r_squared = [0.15, 0.01, 0.18, 0.01, 0.02, 0.03, 0.01, 0.18, 0.09]

R_squared = sum(r_squared)

se = np.zeros(len(beta))
t = np.zeros(len(beta))

for i in xrange(len(beta)):
	se[i] = sd[i]/np.sqrt(n)
	t[i] = beta[i]/se[i]

a = t[1:]*r_squared
print a
print se
print t
print R_squared
