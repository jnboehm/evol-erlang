# -*- coding: utf-8 -*-
from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

from matplotlib.externals import six

import matplotlib as mpl
mpl.use("pgf")
pgf_with_custom_preamble = {
    "font.family": "serif", # use serif/main font for text elements
    "font.size"  : 16,
    "text.usetex": False,   # use inline math for ticks
    "pgf.rcfonts": False,   # don't setup fonts from rc parameters
    "pgf.preamble": [
         "\\usepackage{units}",         # load additional packages
         "\\usepackage{metalogo}",
         "\\usepackage{unicode-math}",  # unicode math setup
         r"\setmathfont{Neo Euler}",
         r"\setmainfont[Numbers={Uppercase,Monospaced}]{Vollkorn}", # serif font via preamble
         ]
}
mpl.rcParams.update(pgf_with_custom_preamble)

import matplotlib.pyplot as plt
import pandas as pd
import re
import numpy
import scipy.optimize as scimin

data = pd.read_csv('ls3opt_nsize10.csv')
data['index'] = data['instance'].map(lambda x: re.search(r'\d+', x).group(0))
data['time_diff_secs'] = data.apply(lambda x: x['time_diff_microseconds'] / 1000000, axis=1)
data = data.groupby(['index'])
time = data.apply(lambda df: df['time_diff_secs'].mean())
datax = time.index.map(lambda x: int(x))
datay = time
x0 = [0]
sigma = numpy.ones(datax.size)
def fitfunc(x,a): # model $f(x)=a x^3$
    return a*x**3
a, b = scimin.curve_fit(fitfunc, datax, datay, x0, sigma)
print(a)

xs = numpy.linspace(0, 180, 100)
ax = plt.figure().add_subplot(1,1,1)
ax.plot(xs, fitfunc(xs, a[0]), color=(0.882352941,0,0.098039216),linewidth=2.2,
        label="$s = a \cdot n^3$")
ax.plot(datax,datay,ls="",marker="x",color=(0.274509804,0.254901961,0.235294118),
        markersize=7, label="Gemessene Werte",
        mew=2.0)
ax.set_ylim([-1,80])


plt.xlabel("Anzahl der Knoten ($n$)")
plt.ylabel("Zeit in Sekunden ($s$)")
plt.legend(loc="upper left")
# plt.legend(["unicode math: $λ=∑_i^∞ μ_i^2$"])
plt.tight_layout(.5)
plt.savefig('ls3opt')
