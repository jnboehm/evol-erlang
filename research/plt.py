import pandas as pd
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
import re
import numpy
import scipy.optimize as scimin
import matplotlib.pyplot as mpl





################################################################
data = pd.read_csv('ls3opt_nsize10.csv')

data['index'] = data['instance'].map(lambda x: re.search(r'\d+', x).group(0))

data = data.groupby(['index'])


time = data.apply(lambda df: df['time_diff_microseconds'].mean())

# plt.scatter(time.index.map(lambda x: int(x)), time, c='blue', alpha=.75)
# # plt.scatter(data['index'], data[3], c='red', alpha=.75)
# plt.show()
################################################################

datax = time.index.map(lambda x: int(x))
datay = time

x0 = [0]

sigma = numpy.ones(datax.size)
print(sigma)

def fitfunc(x,a): # model $f(x)=a x^3$
    return a*x**3

a, b = scimin.curve_fit(fitfunc, datax, datay, x0, sigma)
print(a, b)

xs = numpy.linspace(0, 180, 100)
ax=mpl.figure().add_subplot(1,1,1)

ax.plot(datax,datay,ls="",marker="x",color="blue",mew=2.0,label="Datas")
ax.plot(xs, fitfunc(xs, a[0]))
mpl.show()
