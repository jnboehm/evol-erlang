# -*- coding: utf-8 -*-
from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

from matplotlib.externals import six

import matplotlib as mpl
mpl.use("pgf")
pgf_with_custom_preamble = {
    "font.family": "serif", # use serif/main font for text elements
    "font.size"  : 16,
    "text.usetex": False,   # DON'T use inline math for ticks (set to
                            # True to get the Euler font for the
                            # numerals)
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

################################################################
import matplotlib.pyplot as plt
import pandas as pd
import re
import numpy
import scipy.optimize as scimin
import glob

def make_plot(fname):
    data = pd.read_csv(fname, header=None)
    data.columns = ['gen', 'best', 'current', 'avg']
    data['cur_ratio'] = data.apply(lambda x: x['current'] / x['best'], axis=1)
    data['avg_ratio'] = data.apply(lambda x: x['avg'] / x['best'], axis=1)
    ax = plt.figure().add_subplot(1,1,1)
    ax.plot(data['gen'], data['cur_ratio'], c=(0.882352941,0,0.098039216),
               marker='.', markersize=10, alpha=.75)
    ax.plot(data['gen'], data['avg_ratio'], c=(0.274509804,0.254901961,0.235294118),
               marker='.', markersize=10, alpha=.75)

    plt.tight_layout(.5)
    plt.xlim([-5,105])
    plt.ylim([1.0, 1.7])
    plt.savefig(fname)

def make_all_plots():
    for filename in glob.glob('*-???-??'):
        make_plot(filename)
    for filename in glob.glob('*-???-?'):
        make_plot(filename)

make_all_plots()
