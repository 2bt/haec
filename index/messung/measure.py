#!/usr/bin/env python

import os

SAMPLES = 3
#freqs = [120, 240, 480, 960, 1008]
freqs = [ 30, 48, 60, 72, 84, 96, 120, 132, 144, 156, 168, 180, 192, 204,
	216, 240, 264, 288, 336, 360, 384, 408, 480, 528, 600, 648, 672, 696,
	720, 744, 768, 816, 864, 912, 960, 1008 ]


os.system("""
cpufreq-set --governor userspace
cpufreq-set --min 30000
cpufreq-set --max 1008000
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

		for freq in freqs:
			os.system("cpufreq-set --freq %d000" % freq)
			f = int(file("/sys/devices/system/cpu/cpu0/cpufreq/cpuinfo_cur_freq").read()) / 1000
			assert freq == f, "%d != %d" % (freq, f)

			for input_len in 25, 50, 100:
				t = 0
				for i in range(SAMPLES):
					p = os.popen("../index %s ../wiki/test_%d.txt" % (mode, input_len))
					t += float(p.read())
				t /= SAMPLES

				print " %4d | %7d | %11d | %11d | %9.3f" % (cpus, threads, freq, input_len, t)


os.system("""
cpufreq-set --freq 1008000
echo 1 > /sys/devices/system/cpu/cpu1/online
""")
