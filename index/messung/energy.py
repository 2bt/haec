#!/usr/bin/env python

import sys, os, time, socket

PORT = 1337


def server():
	print "server"

	s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
	s.bind(("", PORT))
	s.listen(1)
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
		os.system("cpufreq-set --min %d" % freq)
		os.system("cpufreq-set --max %d" % freq)
		os.system("cpufreq-set --freq %d" % freq)
		os.system("echo %d > /sys/devices/system/cpu/cpu1/online" % int(cpus == 2))
		time.sleep(1)

		conn.sendall("start") # tell client to begin tracking current
		out = os.popen("../index %s ../wiki/test_%d.txt" % (mode, input_len)).read()
		conn.sendall(out)

		print " %4d | %7d | %11d | %11d | %9.3f" % (
			cpus, 1 + ("t" in mode), freq, input_len, float(out))

	conn.close()



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

	while 1:
		cpus = 2
		mode = "trm"
		freq = 240
		input_len = 50

		# send command
		s.sendall(repr({"cpus":cpus, "mode":mode, "freq":freq, "input_len":input_len}))
		start = s.recv(1024)

		# track current
		s.setblocking(0)
		currents = []
		while 1:
			currents.append(read_current(cambri, 1))
			try:
				time = float(s.recv(1024))
				break
			except:
				continue
		s.setblocking(1)

		# estimate mean
		c = sum(currents) / float(len(currents))
		print " cpus | threads | freq in MHz | input in MB | time in s | current in mA"
		print "------+---------+-------------+-------------+-----------+---------------"
		print " %4d | %7d | %11d | %11d | %9.3f | %013.2f" % (
			cpus, 1 + ("t" in mode), freq, input_len, float(out), c)

		break


	s.close()



if __name__ == "__main__":
	if len(sys.argv) > 1: client(sys.argv[1])
	else: server()
