# coding=utf-8


import pylab as pl
import numpy as np

f = file("table")
#head = [x.strip() for x in next(f).split("|")]
head = [x.split()[0] for x in next(f).split("|")]
print head
next(f)
a = [map(eval,l.split()[::2]) for l in f]


pl.figure(figsize=(10, 6), dpi=80)

Q = sorted(set((x[0],x[1],x[3]) for x in a))
for q in Q:
	X = [x[2] for x in a if (x[0],x[1],x[3]) == q]
	Y = [x[4] for x in a if (x[0],x[1],x[3]) == q]


	#pl.plot(X, Y, "o-", label="CPUS: %d, threads: %d, input: %d MB" % q)
	pl.semilogy(X, Y, "o-", label="CPUS: %d, threads: %d, input: %d MB" % q)

pl.xticks([0]+X)

pl.xlabel(u"Taktfrequenz in MHz")
pl.ylabel(u"Ausführungszeit in s")
pl.legend(loc='upper right', prop={"size": 11})
#pl.savefig("chart.png")
pl.show()
