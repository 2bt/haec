#!/usr/bin/env python
# coding=utf-8


import pylab as pl
import numpy as np

f = file("table")
head = [x.split()[0] for x in next(f).split("|")]
print head
next(f)
a = [map(eval,l.split()[::2]) for l in f]


pl.figure(figsize=(10, 6), dpi=80)

Q = sorted(set((x[0],x[1],x[3]) for x in a))
xticks = None
for q in Q:
	X = [x[2] for x in a if (x[0],x[1],x[3]) == q]
	Y = [x[5] for x in a if (x[0],x[1],x[3]) == q]

	pl.plot(X, Y, "o-", label="CPUS: %d, threads: %d, input: %d MB" % q)
	#pl.semilogy(X, Y, "o-", label="CPUS: %d, threads: %d, input: %d MB" % q)

	if not xticks: xticks = [0] + X

pl.xticks(xticks, [x if x%60==0 else "" for x in xticks])

pl.xlabel(u"Taktfrequenz in MHz")
pl.ylabel(u"Stromstärke in mA")
pl.legend(loc='upper left', prop={"size": 8})
#pl.savefig("chart.png")
pl.show()
