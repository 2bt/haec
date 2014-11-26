#!/usr/bin/env python

import sys, os, time, socket, select, serial

# args: 192.168.1.21 2001


def get_tty_name(c):
	if c == 0:
		if os.path.exists("/sys/devices/platform/sw-ohci.1/usb2/2-1/2-1:1.0/ttyUSB0"):
			return "/dev/ttyUSB0"
		if os.path.exists("/sys/devices/platform/sw-ohci.1/usb2/2-1/2-1:1.0/ttyUSB1"):
			return "/dev/ttyUSB1"
	if c == 1:
		if os.path.exists("/sys/devices/platform/sw-ohci.2/usb4/4-1/4-1:1.0/ttyUSB0"):
			return "/dev/ttyUSB0"
		if os.path.exists("/sys/devices/platform/sw-ohci.2/usb4/4-1/4-1:1.0/ttyUSB1"):
			return "/dev/ttyUSB1"
	return ""


def cambri_init():
	global cambri
	cambri = serial.Serial(get_tty_name(cambri_id / 1000 - 1), 115200, 8, "N", 1)
	for i in range(1, 6):
		cambri.write("en_profile %d %d\r\n" % (i, i == 4))
		cambri_read()


def cambri_read():
	q = ""
	while q[-5:] != "\r\n>> ": q += cambri.read(1)
	return q


def get_current():
	cambri.write("state %d\r\n" % (cambri_id % 1000))
	q = cambri_read()
	return int(q.split("\r\n")[1].split(",")[1])


def main():

	if len(sys.argv) != 3:
		print "usage: %s ip-address cambri-id"
		sys.exit()

	ip_address = sys.argv[1]
	global cambri_id
	cambri_id = int(sys.argv[2])

	cambri_init()

	#cambri.write("state\r\n")
	#print cambri_read()

	if get_current() == 0:
		cambri.write("mode c %d 4\r\n" % (cambri_id % 1000))
		print cambri_read()

	s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
	s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
	s.bind(("", 1337))
	s.listen(1)

	print "waiting for %s to connect..." % ip_address
	while 1:
		conn, addr = s.accept()
		#print "got", addr[0]
		if addr[0] == ip_address: break
		conn.close()

	print "connected."
	print "halting..."

	conn.sendall("halt")
	conn.recv(1024)
	conn.close()

	start_time = time.time()
	while 1:
		current = get_current()
		print "%5.3f %3d" % (time.time() - start_time, current)
		if current == 0: break
		time.sleep(0.05)


	print "off."
	cambri.write("mode o %d 4\r\n" % (cambri_id % 1000))
	cambri_read()

	time.sleep(10)
	cambri.write("mode c %d 4\r\n" % (cambri_id % 1000))
	cambri_read()


	print "on."
	print "waiting for %s to connect..." % ip_address

	start_time = time.time()
	while 1:
		current = get_current()
		print "%5.3f %3d" % (time.time() - start_time, current)

		rl, wl, el = select.select([s], [], [], 0.05)
		if rl:
			conn, addr = s.accept()
			#print "got", addr[0]
			if addr[0] == ip_address: break
		conn.close()

	print "connected."



if __name__ == "__main__": main()
