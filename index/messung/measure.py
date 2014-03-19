#!/usr/bin/env python

import os

SAMPLES = 5

os.system("""
cpufreq-set --governor userspace
cpufreq-set --min 30000
cpufreq-set --max 1010000
""")


print " cpus | threads | freq in MHz | input in MB | time in s"
print "------+---------+-------------+-------------+-----------"



for cpus in [1, 2]:
	if cpus == 1:
		os.system("echo 0 > /sys/devices/system/cpu/cpu1/online")
		modes = ["mr"]
	else:
		modes = ["mr", "tmr"]
		os.system("echo 1 > /sys/devices/system/cpu/cpu1/online")

	for mode in modes:
		threads = 1 if mode == "mr" else 2

		for freq in [120, 240, 480, 960, 1010]:
			os.system("cpufreq-set --freq %d000" % freq)

			for input_len in 25, 50, 100:
				t = 0
				for i in range(SAMPLES):
					p = os.popen("../index %s ../wiki/test_%d.txt" % (mode, input_len))
					t += float(p.read())
				t /= SAMPLES

				print " %4d | %7d | %11d | %11d | %9.3f" % (cpus, threads, freq, input_len, t)


os.system("""
cpufreq-set --freq 1010000
echo 1 > /sys/devices/system/cpu/cpu1/online
""")
