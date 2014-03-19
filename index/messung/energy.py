#!/usr/bin/env python

import sys, os, time, socket

PORT = 1337


def server():
	print "server"
	s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
	s.bind(("", PORT))
	s.listen(1)
	c, addr = s.accept()
	print 'connected by', addr
	while 1:
		data = c.recv(1024)
		if data == "": break

		cmd = eval(data)
		cpus = cmd["cpus"]
		mode = cmd["mode"]
		freq = cmd["freq"]
		input_len = cmd["input_len"]

		print cmd
		os.system("cpufreq-set --governor userspace")
		os.system("cpufreq-set --min %d" % freq)
		os.system("cpufreq-set --max %d" % freq)
		os.system("cpufreq-set --freq %d" % freq)
		os.system("echo %d > /sys/devices/system/cpu/cpu1/online" % int(cpus == 2))

		c.sendall("start")
		out = os.popen("../index %s ../wiki/test_%d.txt" % (mode, input_len)).read()
		c.sendall(out)

		print " cpus | threads | freq in MHz | input in MB | time in s"
		print "------+---------+-------------+-------------+-----------"
		print " %4d | %7d | %11d | %11d | %9.3f" % (
			cpus, 1 + ("t" in mode), freq, input_len, float(out))

	c.close()


def client(host):
	print "client"
	s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
	s.connect((host, PORT))



	s.sendall(repr({
		"cpus": 2,
		"mode": "tmr",
		"freq": 240,
		"input_len": 50
	}))

	start = s.recv(1024)

	time = float(s.recv(1024))

	s.close()



if __name__ == "__main__":
	if len(sys.argv) > 1: client(sys.argv[1])
	else: server()
