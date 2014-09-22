#!/usr/bin/env python

import os
import cherrypy

class BackwardsReader:
	"""Read a file line by line, backwards"""
	BLKSIZE = 4096
	def readline(self):
		while 1:
			newline_pos = self.buf.rfind("\n")
			pos = self.f.tell()
			if newline_pos != -1:
				# Found a newline
				line = self.buf[newline_pos+1:]
				self.buf = self.buf[:newline_pos]
				if pos != 0 or newline_pos != 0 or self.trailing_newline:
					line += "\n"
				return line
			else:
				if pos == 0: # Start-of-file
					return ""
				else:
					# Need to fill buffer
					toread = min(self.BLKSIZE, pos)
					self.f.seek(-toread, 1)
					self.buf = self.f.read(toread) + self.buf
					self.f.seek(-toread, 1)
					if pos - toread == 0:
						self.buf = "\n" + self.buf
	def __init__(self, f):
		self.f = file(f, "r")
		self.buf = ""
		self.f.seek(-1, 2)
		self.trailing_newline = 0
		lastchar = self.f.read(1)
		if lastchar == "\n":
			self.trailing_newline = 1
			self.f.seek(-1, 2)




def get_new_data(max_period, start_time):
	try:
		data = []
		reader = BackwardsReader("../communication/cambri.log")
		newest_time = None
		while 1:
			line = reader.readline()
			if line == "" or line[:1] == "-": break
			columns = line.split()[::2]
			h, m, s = map(float, columns[0].split(":"))
			time = s + m * 60 + h * 3600
			if newest_time == None: newest_time = time
			if time < start_time or newest_time - time > max_period: break
			data.insert(0, [time] + map(int, columns[1:]))

		events = []
		reader = BackwardsReader("../communication/server.log")
		newest_time = None
		while 1:
			line = reader.readline()
			if line == "": break
			fields = line.split()
			h, m, s = map(float, fields[0].split(":"))
			time = s + m * 60 + h * 3600
			if newest_time == None: newest_time = time
			if time < start_time or newest_time - time > max_period: break
			events.insert(0, {"t": time, "e": fields[1], "d": " ".join(fields[2:])[1:-1] })

	except IOError: pass
	return { "current": data, "events": events }



# TESTING
#import time
#import random
#import numpy
#import math
#t_start = time.time()
#data = []
#
#def get_new_data(max_period, start_time):
#	delta_t = 0.5
#
#	a = data[-1][0] + delta_t if data else 0
#	b = time.time() - t_start
#	data.extend([(
#			t,
#			int((1.4 + math.sin(t * 0.1)) * 100 + random.random() * 10 + math.cos(t * 0.7) * 20),
#			random.randint(0, 100)
#		) for t in numpy.arange(a, b, delta_t)])
#
#	i = 0
#	while data[i][0] < start_time: i += 1
#	while data[i][0] < b - max_period: i += 1
#	return data[i:]



class Root:

	@cherrypy.expose
	def index(self):
		return file("html/index.html")

	@cherrypy.expose
	@cherrypy.tools.json_out()
	def poll(self, t=0, m=60.5): return get_new_data(float(m), float(t))


if __name__ == "__main__":

	cherrypy.quickstart(Root(), "", {
		"/": {
			"tools.staticdir.root": os.path.abspath(os.getcwd()),
			"tools.staticfile.root": os.path.abspath(os.getcwd())
		},
		"/static": {
			"tools.staticdir.on": True,
			"tools.staticdir.dir": "static"
		},

		"/favicon.ico": {
			"tools.staticfile.on": True,
			"tools.staticfile.filename": "favicon.ico"
		}
	})
