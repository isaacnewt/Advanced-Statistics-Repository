
#AAcell74
import numpy as np
import math as m
import matplotlib.pyplot as plt
from scipy.stats import binom

#equidistant_bins_x=np.arange(120)
X = []
Y = []

def f(i):
    f = (0.5*0.7)*m.exp(-i*(0.5))  + (0.5*0.29)*m.exp(-i*(0.25))
    return f

for i in range(0,20):
    X.append(i)
    Y.append(f(i))

a = X
d = np.var(a)
mean = np.mean(a)
median = np.median(a)
quartile1 = np.quantile(a, 0.25)
quartile2 = np.quantile(a, 0.5)
quartile3 = np.quantile(a, 0.75)

fig, ax = plt.subplots()
#plt.hist(Y, bins='auto', density=True)
ax.plot(X, Y, label='pdf', c='tab:red', linewidth=3)
bars = plt.bar(X, Y, alpha = 0.5, align ='edge', width =1)
#plt.plot(X, Y, color = 'red')
plt.axvline(mean, label='mean', c='tab:brown')
plt.axvline(median, label='median', c='tab:green')
plt.axvline(quartile1, label='1nd quartile', c='tab:green', linestyle='dotted')
plt.axvline(quartile2, label='2nd quartile', c='tab:green', linestyle='dotted')
plt.axvline(quartile3, label='3rd quartile', c='tab:green', linestyle='dotted')

plt.xticks((min(X), max(X)), color= 'red')
#labx = plt.xlabel('Waiting time to hear owl(mins)')
#laby = plt.ylabel('Probability density function')


ax.set_xlabel('Waiting times to hear owls in minutes', fontsize=15)
ax.set_ylabel('Probability density function', fontsize=15)
ax.set_title('PDF for waiting times to hear owl $t=20$'+"mins", fontsize=15)
ax.legend(loc='best', frameon=True)
ax.grid(True)
plt.savefig("./H.pdf", format='pdf')
plt.show()
