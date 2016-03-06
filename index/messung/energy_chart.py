#!/usr/bin/env python
# coding=utf-8


import pylab as pl
import numpy as np
from matplotlib.legend_handler import HandlerLine2D


f = file("table3")
next(f)
next(f)
a = [map(eval,l.split()[::2]) for l in f]
a = [x for x in a if x[0] > 0 and x[3] == 25]


pl.figure(figsize=(10, 5), dpi=80)
pl.subplots_adjust(bottom=0.2, left=0.1, top=0.9, right=0.95)

hm = {}
for i, q in enumerate(sorted(set((x[0], x[1]) for x in a))):
	X = [x[2] for x in a if tuple(x[:2]) == q]
	Y = [x[5] for x in a if tuple(x[:2]) == q]
	l, = pl.plot(X, Y, "pos*hd"[i], label="%d Kern%s, %d Thread%s" % (q[0], "e"*(q[0]!=1), q[1] + 1, "s"*(q[1]>0)))
	hm[l] = HandlerLine2D(numpoints=1)

xticks = X

pl.xlabel(u"Taktfrequenz in MHz")
pl.ylabel(u"Stromstärke in mA")
pl.legend(loc='upper left', prop={"size": 12}, handler_map=hm)
pl.grid(True, which='major')
pl.xticks(xticks, [240, '', '', '', 360, '', '', 480, '', 600, '', '', '', 720, '', '', 816, '', 912, '', 1008])
pl.xlim(200, 1008 + 40)
#pl.ylim(200, 470)
pl.savefig("cubie-energy.pdf")

pl.show()
