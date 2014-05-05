#!/usr/bin/env python
# coding=utf-8


import pylab as pl
import numpy as np


RASPBERRY = 0
if RASPBERRY:
	f = file("raspberry")
else:
	f = file("table")

head = [x.split()[0] for x in next(f).split("|")]
next(f)
a = [map(eval,l.split()[::2]) for l in f]


pl.figure(figsize=(10, 6), dpi=80)

Q = sorted(set((x[0],x[1],x[3]) for x in a if x[3] >= 0))

xticks = []
for q in Q:
	X = [x[2] for x in a if (x[0],x[1],x[3]) == q]
	Y = [x[5] for x in a if (x[0],x[1],x[3]) == q]
	xticks = max(xticks, [0] + X, key=len)

	pl.plot(X, Y, "o-", label="CPUS: %d, workers: %d, input: %d MB" % q)


pl.xlabel(u"Taktfrequenz in MHz")
pl.ylabel(u"Stromstärke in mA")
pl.legend(loc='lower right', prop={"size": 8})
pl.grid(True, which='major')

if RASPBERRY:
	pl.xticks(xticks, [x if x%100==0 else "" for x in xticks])
	pl.xlim(0, 700)
	pl.ylim(0, 500)
	pl.savefig("pi-energy.svg", dpi=100)
else:
	pl.xticks(xticks, [x if x%60==0 else "" for x in xticks])
	pl.xlim(0, 1008)
	pl.ylim(0, 500)
	pl.savefig("cubie-energy.svg", dpi=200)

pl.show()
