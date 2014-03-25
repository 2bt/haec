#!/usr/bin/env python

import sys, os, time, socket

PORT = 1337
SAMPLES = 3
freqs = [ 30, 48, 60, 72, 84, 96, 120, 132, 144, 156, 168, 180, 192, 204,
	216, 240, 264, 288, 336, 360, 384, 408, 480, 528, 600, 648, 672, 696,
	720, 744, 768, 816, 864, 912, 960, 1008 ]


def server():
	print "server"
	s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
	s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
	s.bind(("", PORT))
	s.listen(1)
	while 1:
		conn, addr = s.accept()
		print "connected by", addr
		while 1:
			data = conn.recv(1024)
			if data == "": break

			print " cpus | threads | freq in MHz | input in MB | time in s"
			print "------+---------+-------------+-------------+-----------"

			cmd = eval(data)
			cpus = cmd["cpus"]
			mode = cmd["mode"]
			freq = cmd["freq"]
			input_len = cmd["input_len"]

			os.system("cpufreq-set --governor userspace")
			os.system("cpufreq-set --min %d000" % freq)
			os.system("cpufreq-set --max %d000" % freq)
			os.system("cpufreq-set --freq %d000" % freq)
			os.system("echo %d > /sys/devices/system/cpu/cpu1/online" % int(cpus == 2))
			time.sleep(1)
			f = int(file("/sys/devices/system/cpu/cpu0/cpufreq/cpuinfo_cur_freq").read()) / 1000
			assert freq == f, "%d != %d" % (freq, f)

			conn.sendall("start") # tell client to begin tracking current
			out = os.popen("../index %s ../wiki/test_%d.txt" % (mode, input_len)).read()
			conn.sendall(out)

			print " %4d | %7d | %11d | %11d | %9.3f" % (
				cpus, 1 + ("t" in mode), freq, input_len, float(out))
		conn.close()



################################################################################



def read_current(cambri, slot):
	cambri.write("state %d\r\n" % slot)
	q = ""
	while q[-5:] != "\r\n>> ": q += cambri.read(1)
	return int(q.split("\r\n")[1].split(",")[1])


def client(host):
	print "client"

	s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
	s.connect((host, PORT))
	import serial
	cambri = serial.Serial("/dev/ttyUSB0", 115200, 8, "N", 1)

	print " cpus | threads | freq in MHz | input in MB | time in s | current in mA"
	print "------+---------+-------------+-------------+-----------+---------------"

	for cpus in [1, 2]:
		modes = ["mr"]
		if cpus == 2, modes.append("tmr")
		for mode in modes:
			for freq in freqs:
				for input_len in 25, 50, 100:
					time = 0
					current = 0
					for i in range(SAMPLES):

						# send command
						s.sendall(repr({"cpus":cpus, "mode":mode, "freq":freq, "input_len":input_len}))
						start = s.recv(1024)

						# track current
						s.setblocking(0)
						currents = []
						while 1:
							currents.append(read_current(cambri, 1))
							try:
								t = float(s.recv(1024))
								break
							except:
								continue
						s.setblocking(1)

						# estimate mean
						c = sum(currents) / float(len(currents))



						time += t
						current += c
					time /= SAMPLES
					current /= SAMPLES

					print " %4d | %7d | %11d | %11d | %9.3f | %13.2f" % (
						cpus, 1 + ("t" in mode), freq, input_len, time, current)

	s.close()



if __name__ == "__main__":
	if len(sys.argv) > 1:
		host = sys.argv[1]
		if host == "-": host = "192.168.1.42"
		client(host)
	else: server()
