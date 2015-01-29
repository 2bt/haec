

var Graph = function(recording_name) {
	var self = this;

	var x_scale = d3.scale.linear().range([0, self.width]);
	var y_scale = d3.scale.linear().range([self.height, 0]);

	self.svg = d3.select("#content").append("svg")
	.attr("width", "100%")
	.attr("viewBox", "0 0 "+ (self.width + margin.left + margin.right) +" "+
	(self.height + margin.top + margin.bottom + margin.middle + margin.row * 7));

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

	svg_yAxis.call(yAxis);
	y_scale.domain([0, 20]);





	$.post("get_recording_data", { "n": recording_name }, function(json) {

		var replay_len = json.power.length;
		x_scale.domain([0, replay_len]);
		// TODO: clip
//		x_scale.domain([70, 30 * 60]);
		xAxis.tickValues(d3.range(0, replay_len, self.tickWidth));
		svg_xAxis.call(xAxis);


		d3.range(json.power[0].length - 1).forEach(function(i) {

			var areas = d3.svg.area()
			.x(function(d) { return x_scale(d[0]); })
			.y0(self.height)
			.y1(function(d) { return y_scale(d3.sum(d.slice(i+1))); });

			var c = colors(i);

			var svg_area = svg_chart.append("path")
			.attr("class", "area")
			.style("fill", d3.rgb(c).brighter())
			.style("stroke", c)
			.datum(json.power)
			.attr("d", areas);

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
		var events = [];
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


		for (var i = 0; i < events.length; i++) {
			var e = events[i];

			if (e.e == "WORK_REQUEST") {
				e.svg.marker.attr("transform", "translate(" + x_scale(e.t) + ",0)");
			}
			if (e.e == "WORK_COMMAND" || e.e == "WORKER_ONLINE" || e.e == "WORKER_ON") {
				var x1 = x_scale(e.t);
				if (x1 < 0) x1 = -1;
				var x2 = self.width;
				if (e.complete) x2 = x_scale(e.complete.t);
				if (x1 < x2) e.svg.rect.attr("x", x1).attr("width", x2 - x1);
			}

		}

	 }, "json");



}

Graph.prototype = Graph;
Graph.width = 1200;
Graph.height = 100;
Graph.tickWidth = 60;

$(document).ready(function() {


	var energy_svg = d3.select("#content").append("svg")
	.attr("width", "100%")
	.attr("viewBox", "0 0 "+ (Graph.width + margin.left + margin.right) +" "+
	(200 + margin.top + margin.bottom + margin.middle));


	var height = 200;

	var x_scale = d3.scale.linear().range([0, Graph.width]);
	var y_scale = d3.scale.linear().range([height, 0]);


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

	var svg_chart = energy_svg.append("g")
	.attr("transform", "translate("+ margin.left +","+ margin.top +")");

	var svg_events = energy_svg.append("g")
	.attr("transform", "translate("+ margin.left +"," + (margin.top + height + margin.middle) +")");

	var svg_events_back_layer = svg_events.append("g");
	var svg_events_front_layer = svg_events.append("g");


	var svg_event_titles = energy_svg.append("g")
	.attr("transform", "translate(" + margin.left_label + "," + (margin.top + height + margin.middle) + ")");


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
	.text("Energy (Ws)");

	svg_yAxis.call(yAxis);
	y_scale.domain([0, 10000]);


	x_scale.domain([0, 60 * 30]);
	xAxis.tickValues(d3.range(0, 60 * 30, Graph.tickWidth));
	svg_xAxis.call(xAxis);


	var g = new Graph("4");
	var h = new Graph("5");



});
