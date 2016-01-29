#!/usr/bin/env python
# coding=utf-8


import pylab as pl
import numpy as np


f = file("table")
next(f)
next(f)
a = [map(eval,l.split()[::2]) for l in f]
a = [x for x in a if x[0] == 2 and x[2] >= 240 and x[3] == 50]


pl.figure(figsize=(10, 5), dpi=80)
pl.subplots_adjust(bottom=0.2, left=0.1, top=0.9, right=0.95)

i = 0
for q in sorted(set(x[1] for x in a)):
	X = [x[2] for x in a if x[1] == q]
	Y = [x[4] for x in a if x[1] == q]
	pl.plot(X, Y, "pos*"[i], label="%d Thread" % (q + 1) + "s"*(i>0))
	i += 1

xticks = X

pl.xlabel(u"Taktfrequenz in MHz")
pl.ylabel(u"Ausführungszeit in s")
pl.legend(loc='upper right', prop={"size": 12})
pl.grid(True, which='major')
pl.xticks(xticks, [240, '', '', '', 360, '', '', 480, '', 600, '', '', '', 720, '', '', 816, '', 912, '', 1008])
pl.xlim(200, 1008 + 40)
pl.ylim(0, 100)
pl.savefig("cubie-time.pdf")

#pl.show()
