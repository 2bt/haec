

var Graph = function(recording_name, ready_function, recording_title) {
	var self = this;

	var x_scale = d3.scale.linear().range([0, self.width]);
	var y_scale = d3.scale.linear().range([self.height, 0]);

	self.svg = d3.select("#content").append("svg")
	.attr("width", "100%")
	.attr("viewBox", "0 0 "+ (self.width + margin.left + margin.right) +" "+
	(self.height + margin.top + margin.bottom + margin.middle + margin.row * 7));

	var clip = self.svg.append("defs").append("svg:clipPath")
	.attr("id", "clip")
	.append("svg:rect")
	.attr("width", self.width)
	.attr("height", 1000);


	var format = d3.format("02d");
	var xAxis = d3.svg.axis()
	.scale(x_scale)
	.orient("bottom")
	.tickFormat(function(t) {
		if (t < 0) return "";
		var h = Math.floor(t / 3600);
		var m = Math.floor(t / 60) % 60;
		var s = t % 60;
		if (self.tickWidth % 60 == 0) return h+":"+format(m);
		else return h+":"+format(m)+":"+format(s);
	});
	var yAxis = d3.svg.axis()
	.scale(y_scale)
	.orient("left");

	var svg_chart = self.svg.append("g")
	.attr("transform", "translate("+ margin.left +","+ margin.top +")");

	var svg_events = self.svg.append("g")
	.attr("clip-path", "url(#clip)")
	.attr("transform", "translate("+ margin.left +"," + (margin.top + self.height + margin.middle) +")");

	var svg_events_back_layer = svg_events.append("g");
	var svg_events_front_layer = svg_events.append("g");


	var svg_event_titles = self.svg.append("g")
	.attr("transform", "translate(" + margin.left_label + "," + (margin.top + self.height + margin.middle) + ")");


	var svg_xAxis = svg_chart.append("g")
	.attr("class", "x axis")
	.attr("transform", "translate(0," + self.height + ")")

	var svg_yAxis = svg_chart.append("g")
	.attr("class", "y axis");

	svg_yAxis.append("text")
	.attr("transform", "rotate(-90)")
	.attr("y", 6)
	.attr("dy", ".71em")
	.style("text-anchor", "end")
	.text("Power (W)");

	y_scale.domain([0, 20]);
	svg_yAxis.call(yAxis);


	svg_chart.append("text")
		.attr("x", self.width / 2)
		.attr("y", 0)
		.attr("dy", ".45em")
		.style("text-anchor", "middle")
		.attr("class", "chart-title")
		.text(recording_title);

	var events = [];
	var svg_areas = [];
	var areas = [];

	self.events = events;
	self.svg_areas = svg_areas;
	self.areas = areas;
	self.xAxis = xAxis;
	self.x_scale = x_scale;
	self.svg_xAxis = svg_xAxis;

	$.post("get_recording_data", { "n": recording_name }, function(json) {
		self.json = json;

		self.recording_length = json.power.length;


		d3.range(json.power[0].length - 1).forEach(function(i) {

			areas[i] = d3.svg.area()
			.x(function(d) { return x_scale(d[0]); })
			.y0(self.height)
			.y1(function(d) { return y_scale(d3.sum(d.slice(i+1))); });

			var c = colors(i);

			svg_areas[i] = svg_chart.append("path")
			.attr("clip-path", "url(#clip)")
			.attr("class", "area")
			.style("fill", d3.rgb(c).brighter())
			.style("stroke", c)
			.datum(json.power)
			.attr("d", areas[i]);

			if (i < 6) {
				// lanes
				svg_events_front_layer.append("rect")
				.attr("width", self.width)
				.attr("height", margin.row)
				.attr("y", (i + 1) * margin.row)
				.style("fill", d3.rgb(c).brighter())
				.style("fill-opacity", 0.1);

				// lane titles
				svg_event_titles.append("text")
				.attr("text-anchor", "end")
				.attr("y",(i + 1.5) * margin.row)
				.attr("dy",".5ex")
				.text(lane_titles[i]);
			}
		});



		// events
		json.events.forEach(function(e, i) {
			// process only these events
			if (!{
				"WORK_REQUEST": true,
				"WORK_COMMAND": true,
				"WORK_COMPLETE": true,
				"WORKER_ON": true,
				"WORKER_ONLINE": true,
				"WORKER_OFF": true,
				"WORKER_OFFLINE": true,
			}[e.e]) return;


			// construct info
			e.info = {}
			e.d.split("; ").forEach(function(l) {
				l = l.split(": ")
				e.info[l[0]] = l[1];
			});

			e.svg = {};


			if (e.e == "WORK_REQUEST") {
				events.push(e);
				e.svg.marker = svg_events_front_layer.append("polyline")
				.attr("class", "marker")
				.attr("points", "0,0, 0,10");

			}
			else if (e.e == "WORK_COMMAND") {
				events.push(e);

				var y_scale = (id_to_index(e.info.id) + 1) * margin.row + 3;

				e.svg.rect = svg_events_front_layer.append("rect")
				.attr("class", "work-command")
				.attr("y", y_scale)
				.attr("height", margin.row - 6);

				events.forEach(function(f) {
					if (f.e == "WORK_REQUEST" && f.info["work-id"] == e.info["work-id"]) {
						e.request = f;
						f.command = e;
						return;
					}
				});
			}
			else if (e.e == "WORK_COMPLETE") {
				events.forEach(function(f) {
					if (f.e == "WORK_COMMAND" && f.info["work-id"] == e.info["work-id"]) {
						e.command = f;
						f.complete = e;
					}
				});
			}
			else if (e.e == "WORKER_ON") {
				// only consider event, if no previous WORKER_ON event was emited
				var i;
				for (i = 0; i < events.length; i++) {
					var f = events[i];
					if (f.e == "WORKER_ON" && f.info["id"] == e.info["id"] && !f.complete) {
						break;
					}
				}
				if (i == events.length) {
					var i = id_to_index(e.info.id);
					events.push(e);
					e.svg.rect = svg_events_back_layer.append("rect")
					.attr("y", (i + 1) * margin.row)
					.attr("height", margin.row)
					.style("fill-opacity", 0.8)
					.style("stroke", "none")
					.style("fill", d3.rgb(colors(i)).brighter(1.8));
				}
			}
			else if (e.e == "WORKER_ONLINE") {
				var i = id_to_index(e.info.id);
				events.push(e);
				e.svg.rect = svg_events_back_layer.append("rect")
				.attr("y", (i + 1) * margin.row)
				.attr("height", margin.row)
				.style("stroke", "none")
				.style("fill-opacity", 0.8)
				.style("fill", d3.rgb(colors(i)));

			}
			else if (e.e == "WORKER_OFF") {
				events.forEach(function(f) {
					if (f.e == "WORKER_ON" && f.info["id"] == e.info["id"] && !f.complete) {
						f.complete = e;
					}
				});
			}
			else if (e.e == "WORKER_OFFLINE") {
				events.forEach(function(f) {
					if (f.e == "WORKER_ONLINE" && f.info["id"] == e.info["id"] && !f.complete) {
						f.complete = e;
					}
				});
			}
		});

		self.set_x_domain();

		ready_function(self);

	 }, "json");


};

Graph.prototype = Graph;
Graph.width = 1200;
Graph.height = 100;
Graph.tickWidth = 60;

Graph.set_x_domain = function(a, b) {
	var self = this;

	if (a === undefined) {
		a = 0;
		b = self.recording_length;
	}


	self.x_scale.domain([a, b]);
	var l = d3.max([0, Math.ceil(a / self.tickWidth) * self.tickWidth]);
	self.xAxis.tickValues(d3.range(l, b, self.tickWidth));
	self.svg_xAxis.call(self.xAxis);

	for (var i = 0; i < self.areas.length; i++) {
		self.svg_areas[i].attr("d", self.areas[i]);
	}

	self.events.forEach(function(e) {

		if (e.e == "WORK_REQUEST") {
			e.svg.marker.attr("transform", "translate(" + self.x_scale(e.t) + ",0)");
		}
		if (e.e == "WORK_COMMAND" || e.e == "WORKER_ONLINE" || e.e == "WORKER_ON") {
			var x1 = self.x_scale(e.t);
			if (x1 < 0) x1 = -1;
			var x2 = self.width;
			if (e.complete) x2 = self.x_scale(e.complete.t);
			if (x1 < x2) e.svg.rect.attr("x", x1).attr("width", x2 - x1);
		}

	});
};


var Energy = function() {
	var self = this;

	var svg = d3.select("#content").append("svg")
	.attr("width", "100%")
	.attr("viewBox", "0 0 "+ (Graph.width + margin.left + margin.right) +" "+
	(200 + margin.top + margin.bottom + margin.middle));


	var height = 200;
	var x_scale = d3.scale.linear().range([0, Graph.width]);
	var y_scale = d3.scale.linear().range([height, 0]);

	var clip = svg.append("defs").append("svg:clipPath")
	.attr("id", "clip")
	.append("svg:rect")
	.attr("width", self.width)
	.attr("height", height);

	var format = d3.format("02d");
	var xAxis = d3.svg.axis()
	.scale(x_scale)
	.orient("bottom")
	.tickFormat(function(t) {
		if (t < 0) return "";
		var h = Math.floor(t / 3600);
		var m = Math.floor(t / 60) % 60;
		var s = t % 60;
		if (Graph.tickWidth % 60 == 0) return h+":"+format(m);
		else return h+":"+format(m)+":"+format(s);
	});
	var yAxis = d3.svg.axis()
	.scale(y_scale)
	.orient("left");

	var svg_chart = svg.append("g")
	.attr("transform", "translate("+ margin.left +","+ margin.top +")");

	var svg_xAxis = svg_chart.append("g")
	.attr("class", "x axis")
	.attr("transform", "translate(0," + height + ")")

	var svg_yAxis = svg_chart.append("g")
	.attr("class", "y axis");
	svg_yAxis.append("text")
	.attr("transform", "rotate(-90)")
	.attr("y", 6)
	.attr("dy", ".71em")
	.style("text-anchor", "end")
	.text("Consumed energy (Ws)");

	y_scale.domain([0, self.max_energy]);
	svg_yAxis.call(yAxis);


	self.x_scale = x_scale;
	self.y_scale = y_scale;

	self.xAxis = xAxis;
	self.svg_xAxis = svg_xAxis;

	self.yAxis = yAxis;
	self.svg_yAxis = svg_yAxis;

	self.svg_chart = svg_chart;


	self.lines = [];
	self.svg_lines = [];


};


Energy.prototype = Energy;

Energy.max_energy = 18000;
Energy.set_x_domain = function(t1, t2) {
	var self = this;


	self.x_scale.domain([t1, t2]);
	var l = d3.max([0, Math.ceil(t1 / Graph.tickWidth) * Graph.tickWidth]);
	self.xAxis.tickValues(d3.range(l, t2, Graph.tickWidth));
	self.svg_xAxis.call(self.xAxis);


	for (var i = 0; i < self.lines.length; i++) {
		self.svg_lines[i].attr("d", self.lines[i]);
	}


};


Energy.add_line = function(statuses, color, name) {
	var self = this;

	var i = self.lines.length;

	self.lines[i] = d3.svg.line()
	.interpolate("basic")
	.x(function(d, i) { return self.x_scale(d.t); })
	.y(function(d) {
		return self.y_scale(d.energy * 1);
	});

	self.svg_lines[i] = self.svg_chart.append("path")
	.attr("clip-path", "url(#clip)")
	.attr("class", "line")
	.attr("stroke", color)
	.datum(statuses)
	.attr("d", self.lines[i]);

	self.svg_chart.append("rect")
		.attr("x",50)
		.attr("y",i*25)
		.attr("width", 20)
		.attr("height", 20)
		.attr("fill", color);
	self.svg_chart.append("text")
		.attr("x", 72)
		.attr("y", i*25 + 10)
		.attr("dy", ".45em")
		.text(name);

};





$(document).ready(function() {

	e = new Energy();
	graphs = [];

	var domain = 21*60 + 10;

	g = new Graph("x4", function(g){

		e.set_x_domain(0, domain);
		e.add_line(g.json.statuses, "orange", "simple scheduler with adaptive switching of workers");

	}, "simple scheduler with adaptive switching of workers");

	i = new Graph("x6", function(i){

		e.set_x_domain(0, domain);
		h.set_x_domain(0, domain);
		g.set_x_domain(0, domain);
		i.set_x_domain(0, domain);

		e.add_line(i.json.statuses, "red", "simple scheduler");

	}, "simple scheduler");
	h = new Graph("x2", function(h){

		e.set_x_domain(0, domain);
		h.set_x_domain(0, domain);
		g.set_x_domain(0, domain);

		e.add_line(h.json.statuses, "green", "load-consolidating scheduler with adaptive switching of workers");

	}, "load-consolidating scheduler with adaptive switching of workers");


/*	j = new Graph("d4", function(j){

		e.set_x_domain(0, g.recording_length);
		h.set_x_domain(0, g.recording_length);
		g.set_x_domain(0, g.recording_length);
		i.set_x_domain(0, g.recording_length);
		j.set_x_domain(0, g.recording_length);

		e.add_line(j.json.statuses, "#ff0", "run 4");

	});

	k = new Graph("d5", function(j){

		e.set_x_domain(0, g.recording_length);
		h.set_x_domain(0, g.recording_length);
		g.set_x_domain(0, g.recording_length);
		i.set_x_domain(0, g.recording_length);
		j.set_x_domain(0, g.recording_length);
		k.set_x_domain(0, g.recording_length);

		e.add_line(k.json.statuses, "#777");

	});*/ 

});

