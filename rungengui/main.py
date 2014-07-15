#!/usr/bin/python

import wx
import math
import re
import random


def formated_time_to_seconds(t):
	match = re.match(r"^(\d\d):(\d\d):(\d\d)$", t)
	return (int(match.group(1)) * 60 * 60 +
			int(match.group(2)) * 60 +
			int(match.group(3)))


def seconds_to_formated_time(s):
	m = math.floor(s / 60.0)
	s %= 60
	h = math.floor(m / 60.0)
	m %= 60
	return "%02d:%02d:%02d" % (h, m, s)


def generate(path, data, disp):
	def get(points, t):
		i = 0
		a = points[0]
		for p in points[1:]:
			b = p
			if p[0] >= t: break
			a = p
		if a == b: return a[1]
		return a[1] + (t - a[0]) / float(b[0] - a[0]) * (b[1] - a[1])

	freq, size, time = data
	_, size_disp, time_disp = disp
	total_time = freq[-1][0]

	f = open(path, "w")
	t = 0
	while t < total_time:
		s = max(0, get(size, t) + random.uniform(-size_disp, size_disp))
		l = max(0, get(time, t) + random.uniform(-time_disp, time_disp))
		f.write("%s %d %d\n" % (seconds_to_formated_time(t), s, t))
		t += 60 * 60.0 / max(get(freq, t), 1)
	f.close()


class Chart(wx.Panel):
	def __init__(self, parent, type):
		super(wx.Panel, self).__init__(parent)
		self.font = wx.Font(8, wx.MODERN, wx.NORMAL, wx.NORMAL, False, "mono")
		self.Bind(wx.EVT_PAINT, self.OnPaint)
		self.Bind(wx.EVT_SIZE, self.OnSize)
		self.Bind(wx.EVT_MOUSE_EVENTS, self.OnMouse)

		self.marked = None
		self.x_text = wx.StaticText(parent, size=(60, -1))
		self.y_text = wx.StaticText(parent, size=(50, -1), style=wx.ALIGN_RIGHT)
		self.y_range_tc = wx.TextCtrl(parent, style=wx.TE_PROCESS_ENTER, size=(60, -1))
		self.y_range_tc.Bind(wx.EVT_TEXT_ENTER, self.OnYRangeTextEnter)

		if type != "freq":
			self.data = [(0, 0), (1, 1)]
			self.dispersion_tc = wx.TextCtrl(parent, style=wx.TE_PROCESS_ENTER, size=(35, -1))
			self.dispersion_tc.SetValue("0")
		else:
			self.data = [(0, 0.5), (1, 0.5)]
			self.dispersion_tc = None

		self.set_x_range(60 * 60)

		self.type = type
		if type == "freq":
			self.color = "#ff0000"
			self.y_range = 60
			self.y_inc = 10
		elif type == "size":
			self.color = "#00aa00"
			self.y_range = 384
			self.y_inc = 32
		else:
			self.color = "#0000ff"
			self.y_range = 60 * 60
			self.y_inc = 60 * 10

		self.set_y_range(self.y_range)

	def set_y_range(self, v):
		self.y_range = v
		if self.type == "time":
			self.y_range_tc.SetValue(seconds_to_formated_time(v))
		else:
			self.y_range_tc.SetValue("%d" % v)
		self.Refresh()

	def set_x_range(self, v):
		self.x_range = v
		self.Refresh()

	def OnYRangeTextEnter(self, event):
		val = event.GetEventObject().GetValue()
		try:
			if self.type == "time":
				self.y_range = formated_time_to_seconds(val)
			else:
				self.y_range = int(val)
			self.Refresh()
		except:pass

	def closest(self, x, y):
		w, h = self.GetSize()
		ret = None
		dist = 9e9
		for i, p in enumerate(self.data):
			d = (p[0]*w - x) ** 2 + ((1-p[1])*h - y) ** 2
			if d < dist:
				dist = d
				ret = i
		return ret, dist**0.5

	def OnMouse(self, event):
		w, h = self.GetSize()
		x, y = event.GetPositionTuple()


		# update labels
		if event.Leaving():
			if self.x_text: self.x_text.SetLabel("")
			if self.y_text: self.y_text.SetLabel("")
		if event.Moving() or event.Dragging():
			p = (x / float(w), 1 - y / float(h))
			xl = seconds_to_formated_time(p[0] * self.x_range)
			if self.type == "time":
				yl = seconds_to_formated_time(p[1] * self.y_range)
			else:
				yl = "%d" % math.floor(p[1] * self.y_range)
			if self.x_text: self.x_text.SetLabel(xl)
			if self.y_text: self.y_text.SetLabel(yl)


		if event.LeftDown():
			self.marked = None
			i, d = self.closest(x, y)
			if d <= 8:
				self.marked = i
				self.Refresh()

		elif event.Dragging():
			if self.marked != None:
				mx = max(0, min(x/float(w), 1))
				my = max(0, min(1 - y/float(h), 1))
				if self.marked == 0 or self.marked == len(self.data) - 1:
					mx = self.data[self.marked][0]

				self.data[self.marked] = (mx, my)
				self.data.sort(key=lambda p:p[0])
				self.marked = self.closest(x, y)[0]
				self.Refresh()

		elif event.LeftDClick():
			mx = max(0, min(x/float(w), 1))
			my = max(0, min(1 - y/float(h), 1))

			self.data.append((mx, my))
			self.data.sort(key=lambda p:p[0])
			self.marked = self.closest(x, y)[0]
			self.Refresh()

		elif event.RightDClick():
			self.marked = None
			i, d = self.closest(x, y)
			if d <= 8 and i > 0 and i < len(self.data) - 1:
				del self.data[i]
				self.Refresh()

		elif not event.LeftIsDown():
			if self.marked != None:
				self.marked = None
				self.Refresh()

	def OnSize(self, event):
		self.Refresh()

	def OnPaint(self, event):
		w, h = self.GetSize()
		dc = wx.PaintDC(self)

		dc.SetFont(self.font)

		dc.Clear()

		pen1 = wx.Pen("#afafaf", 1, wx.SOLID)
		pen2 = wx.Pen("#dfdfdf", 1, wx.SOLID)

		dc.SetPen(pen1)
		for y in range(0, self.y_range + 1, self.y_inc):
			v = h - y / float(self.y_range) * h
			dc.DrawLine(0, v, w, v)

		for x in range(0, self.x_range + 1, 60):
			u = x / float(self.x_range) * w
			if x % (60 * 60) == 0: dc.SetPen(pen1)
			else: dc.SetPen(pen2)
			dc.DrawLine(u, 0, u, h)

		red_pen = wx.Pen(self.color, 2, wx.SOLID)
		black_pen = wx.Pen("#000000", 3, wx.SOLID)
		red_pen.SetJoin(wx.JOIN_ROUND)

		for i, p2 in enumerate(self.data):

			x2 = p2[0] * w
			y2 = (1 - p2[1]) * h

			dc.SetPen(black_pen)
			if i == self.marked:
				dc.DrawCircle(x2, y2, 5)
			else:
				dc.DrawLine(x2 - 5, y2, x2 + 5, y2)
				dc.DrawLine(x2, y2 - 5, x2, y2 + 5)


			dc.SetPen(red_pen)
			if i > 0:
				dc.DrawLine(x1, y1, x2, y2)

			x1 = x2
			y1 = y2

		if self.marked != None: p = self.data[self.marked]


class Win(wx.Frame):

	def __init__(self):
		super(Win, self).__init__(None, -1, "rungengui")

		file_menu = wx.Menu()
		menu_open = file_menu.Append(-1, "&Open scenario")
		menu_save = file_menu.Append(-1, "&Save scenario")
		file_menu.AppendSeparator()
		menu_quit = file_menu.Append(-1, "&Quit")

		self.Bind(wx.EVT_MENU, self.OnExit, menu_quit)
		self.Bind(wx.EVT_MENU, self.OnOpen, menu_open)
		self.Bind(wx.EVT_MENU, self.OnSave, menu_save)

		menubar = wx.MenuBar()
		menubar.Append(file_menu, '&File')
		self.SetMenuBar(menubar)


		panel = wx.Panel(self, -1)
		self.charts = [
			Chart(panel, "freq"),
			Chart(panel, "size"),
			Chart(panel, "time")
		]

		vbox = wx.BoxSizer(wx.VERTICAL)
		vbox.Add((-1, 10))

		hbox = wx.BoxSizer(wx.HORIZONTAL)
		hbox.Add(wx.StaticText(panel, label="Total time:"))
		self.total_time_tc = wx.TextCtrl(panel, style=wx.TE_PROCESS_ENTER, size=(60, -1))
		self.total_time_tc.Bind(wx.EVT_TEXT_ENTER, self.OnTotalTimeTextEnter)
		hbox.Add(self.total_time_tc, flag=wx.LEFT, border=10)
		hbox.Add((-1, -1), proportion=1)
		b = wx.Button(panel, label="Generate")
		b.Bind(wx.EVT_BUTTON, self.OnGenerate)
		hbox.Add(b, flag=wx.LEFT, border=10)
		vbox.Add(hbox, flag=wx.EXPAND|wx.LEFT|wx.RIGHT|wx.TOP, border=10)

		vbox.Add((-1, 10))

		self.total_time_tc.SetValue("01:00:00")

		for i in range(3):
			hbox = wx.BoxSizer(wx.HORIZONTAL)
			hbox.Add(wx.StaticText(panel, label=[
				"Request frequency (limit: ",
				"Request size (limit: ",
				"Request time (limit: "
			][i]))
			hbox.Add(self.charts[i].y_range_tc)
			hbox.Add(wx.StaticText(panel, label=[
				" requests per hour)",
				" MB, dispersion: ",
				" , dispersion: "
			][i]))
			if i > 0:
				hbox.Add(self.charts[i].dispersion_tc)
				hbox.Add(wx.StaticText(panel, label=" %)"))

			hbox.Add((-1, -1), proportion=1)
			hbox.Add(self.charts[i].x_text)
			hbox.Add(self.charts[i].y_text)
			vbox.Add(hbox, flag=wx.EXPAND|wx.LEFT|wx.RIGHT, border=10)

			hbox = wx.BoxSizer(wx.HORIZONTAL)
			hbox.Add(self.charts[i], proportion=1, flag=wx.EXPAND|wx.LEFT|wx.RIGHT, border=10)
			vbox.Add(hbox, proportion=1, flag=wx.EXPAND)
			vbox.Add((-1, 10))

		panel.SetSizer(vbox)

		self.Center()
		self.Show()

	def OnTotalTimeTextEnter(self, event):
		val = event.GetEventObject().GetValue()
		s = formated_time_to_seconds(val)
		if not s: return
		for c in self.charts: c.set_x_range(s)

	def OnGenerate(self, event):
		data = [[(x*c.x_range, y*c.y_range) for x,y in c.data] for c in self.charts]
		disp = []
		for c in self.charts:
			try: d = float(c.dispersion_tc.GetValue()) / 100 * c.y_range
			except: d = 0
			disp.append(d)
		dialog = wx.FileDialog(self, "Save generated run", "", "", "*.run", wx.FD_SAVE)
		if dialog.ShowModal() == wx.ID_OK: generate(dialog.GetPath(), data, disp)
		dialog.Destroy()

	def OnOpen(self, event):
		dialog = wx.FileDialog(self, "Open scenario", "", "", "*.scenario", wx.FD_OPEN)
		if dialog.ShowModal() == wx.ID_OK:
			f = open(dialog.GetPath())
			for c in self.charts:
				x, y, d, disp = eval(f.readline())
				c.x_range = x
				c.data = d
				c.set_y_range(y)
				if c.dispersion_tc:
					c.dispersion_tc.SetValue("%d" % disp)
			f.close()
			self.total_time_tc.SetValue(seconds_to_formated_time(self.charts[0].x_range))
		dialog.Destroy()

	def OnSave(self, event):
		dialog = wx.FileDialog(self, "Save scenario", "", "", "*.scenario", wx.FD_SAVE)
		if dialog.ShowModal() == wx.ID_OK:
			f = open(dialog.GetPath(), "w")
			for c in self.charts:
				try: disp = float(c.dispersion_tc.GetValue())
				except: disp = 0
				f.write("%r\n" % [c.x_range, c.y_range, c.data, disp])
			f.close()
		dialog.Destroy()

	def OnExit(self, event):
		self.Close()


if __name__ == "__main__":
	app = wx.App()
	Win()
	app.MainLoop()
