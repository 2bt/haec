#!/usr/bin/env python

import sys, os, time, socket

PORT = 1337
SAMPLES = 3
freqs = [
	#30, 48, 60, 72, 84, 96, 120, 132, 144, 156, 168, 180, 192,
	#204, 216,
	240, 264, 288, 336, 360, 384, 408, 480, 528, 600, 648, 672, 696,
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

			print " cpus | workers | freq in MHz | input in MB | time in s"
			print "------+---------+-------------+-------------+-----------"

			cmd = eval(data)
			cpus = cmd["cpus"]
			workers = cmd["workers"]
			freq = cmd["freq"]
			input_len = cmd["input_len"]

			os.system("cpufreq-set --governor userspace")
			os.system("cpufreq-set --min 30000")
			if socket.gethostname() == "raspberrypi":
				os.system("cpufreq-set --max 700000")
			else:
				os.system("cpufreq-set --max 1008000")
				os.system("echo %d > /sys/devices/system/cpu/cpu1/online" % int(cpus == 2))
			os.system("cpufreq-set --freq %d000" % freq)
			time.sleep(1)
			f = int(file("/sys/devices/system/cpu/cpu0/cpufreq/cpuinfo_cur_freq").read()) / 1000
			assert freq == f, "%d != %d" % (freq, f)

			conn.sendall("start") # tell client to begin tracking current
			if input_len > 0:
				out = os.popen("../index mr %d ../wiki/test_%d.txt" % (workers, input_len)).read()
				conn.sendall(out)
			else:
				time.sleep(20)
				out = "0"
				conn.sendall(out)


			print " %4d | %7d | %11d | %11d | %9.3f" % (
				cpus, workers, freq, input_len, float(out))
			print
		conn.close()



################################################################################

class Tee(object):
	def __init__(self, name="out", mode="a"):
		self.file = open(name, mode)
		self.stdout = sys.stdout
		sys.stdout = self
	def __del__(self):
		sys.stdout = self.stdout
		self.file.close()
	def write(self, data):
		self.file.write(data)
		self.stdout.write(data)


def read_current(cambri, slot):
	cambri.write("state %d\r\n" % slot)
	q = ""
	while q[-5:] != "\r\n>> ": q += cambri.read(1)
	return int(q.split("\r\n")[1].split(",")[1])


def client(host):
	print "client"
	tee = Tee()

	s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
	s.connect((host, PORT))
	import serial
	cambri = serial.Serial("/dev/ttyUSB0", 115200, 8, "N", 1)

	print " cpus | workers | freq in MHz | input in MB | time in s | current in mA"
	print "------+---------+-------------+-------------+-----------+---------------"

	for input_len in [50]:
		for cpus in [1, 2]:
			for workers in [0, 1, 2]:
				for freq in freqs:
					time = 0
					current = 0
					for i in range(SAMPLES):

						# send command
						s.sendall(repr({
							"cpus": cpus,
							"workers": workers,
							"freq": freq,
							"input_len": input_len
						}))
						start = s.recv(1024)

						# track current
						s.setblocking(0)
						currents = []
						while 1:
							currents.append(read_current(cambri, 1))
							try:
								t = float(s.recv(1024))
								break
							except: continue
						s.setblocking(1)

						# estimate mean
						c = sum(currents) / float(len(currents))


						time += t
						current += c
					time /= SAMPLES
					current /= SAMPLES

					print " %4d | %7d | %11d | %11d | %9.3f | %13.2f" % (
						cpus, workers, freq, input_len, time, current)

	s.close()



if __name__ == "__main__":
	if len(sys.argv) > 1:
		host = sys.argv[1]
		if host == "-": host = "192.168.1.42"
		client(host)
	else: server()
