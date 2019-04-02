import numpy as np
import matplotlib.pyplot as plt

age_V = [29., 27., 28., 26.]
weight_V = [13.1, 12.4, 13.2, 11.8]
age_W = [21., 27., 29., 23., 25.]
weight_W = [11.5, 14.2, 15.4, 13.1, 13.8]


plt.plot(age_V, weight_V, 'ro')
plt.plot(age_W, weight_W, 'bo')
plt.legend(['Virgina', 'Wisconsin'])
plt.title('Weights at slaughter')
plt.xlabel('Age [weeks]')
plt.ylabel('Weight [lbs]')
plt.grid()
plt.show()

