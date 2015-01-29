$(document).ready(function() {

	$("form,.cmd").submit(function() {
		var p = $(this).parent();
		p.css("background-color", "red");

		var q = $(this).serialize();
		//$("input[name=cmd]", this).val("");

		$.post("command", q, function(ret) {
			p.css("background-color", "");
		});
		return false;
	});





	var past = 60 * 5;
	var tickWidth = 60;

	var delta = 1; // one sample per second
	var update = 1;
	var power = [];
	var events = [];
	var time = -past;
	var width = 600;
	var height = 400;


	var x = d3.scale.linear()
	.range([0, width]);

	var y = d3.scale.linear()
	.range([height, 0]);

	var format = d3.format("02d");
	var xAxis = d3.svg.axis()
	.scale(x)
	.orient("bottom")
	.tickFormat(function(t) {
		if (t < 0) return "";
		var h = Math.floor(t / 3600);
		var m = Math.floor(t / 60) % 60;
		var s = t % 60;
		if (tickWidth % 60 == 0) return h+":"+format(m);
		else return h+":"+format(m)+":"+format(s);
	});

	var yAxis = d3.svg.axis()
	.scale(y)
	.orient("left");

	var svg = d3.select("#svg")
	.attr("width", width + margin.left + margin.right)
	.attr("height", height + margin.top + margin.bottom);

	var svg_chart = d3.select("#chart")
	.attr("transform", "translate("+ margin.left +","+ margin.top +")");

	var svg_events = d3.select("#events")
	.attr("transform", "translate("+ margin.left +"," + (margin.top + height + margin.middle) +")");

	d3.select("#cliprect")
	.attr("width", width)
	.attr("height", 1000);
	var svg_events_back_layer = svg_events.append("g");
	var svg_events_front_layer = svg_events.append("g");


	var svg_event_titles = d3.select("#event_titles")
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
	.text("Power (W)");


	x.domain([-past, 0]);
	y.domain([0, 20]);
	svg_xAxis.call(xAxis);
	svg_yAxis.call(yAxis);

	var svg_areas = [];
	var areas = [];


	var first_poll = true;



	(function poll() {
		$.post("poll", { "t": time, "m": past + delta }, function(json) {
			console.log("poll", json);
			$("#e-meter").text(json.status.energy);
			$("#currentsched").text(json.status.scheduler);
			if (json.power.length > 0) {
				if (first_poll) {
					first_poll = false;

					svg.attr("height", height + margin.top + margin.bottom + margin.middle + margin.row * 7);

					d3.range(8).forEach(function(i) {

						areas[i] = d3.svg.area()
						.x(function(d) { return x(d[0]); })
						.y0(height)
						.y1(function(d) { return y(d3.sum(d.slice(i+1))); });

						var c = colors(i);

						svg_areas[i] = svg_chart.append("path")
						.attr("class", "area")
						.style("fill", d3.rgb(c).brighter())
						.style("stroke", c)
						.datum(power)
						.attr("d", areas[i]);


						if (i < 6) {
							// lanes
							svg_events_front_layer.append("rect")
							.attr("width", width)
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

				}


				json.power.forEach(function(d) { power.push(d); });
				while (power.length > past / delta + 1) power.shift();
				time = power[power.length - 1][0];


				x.domain([time - past, time]);
				var l = d3.max([0, Math.ceil((time - past) / tickWidth) * tickWidth]);
				xAxis.tickValues(d3.range(l, time, tickWidth));
				svg_xAxis.call(xAxis);


				for (var i = 0; i < areas.length; i++) {
					svg_areas[i].attr("d", areas[i])
				}



				// events
				json.events.forEach(function(e, i) {

					// console output
					var h = format(Math.floor(e.t / 3600));
					var m = format(Math.floor(e.t / 60) % 60);
					var s = format(Math.floor(e.t % 60));
					var ms = format(Math.round(e.t % 1 * 100));
					var time = h + ":" + m + ":" + s + "." + ms;
					$("<pre></pre>").text(time +" "+ e.e + Array(15 - e.e.length).join(" ") +" ("+ e.d +")").prependTo("#console");


					// process only these events
					if (!{
						"WORK_REQUEST": true,
						"WORK_COMMAND": true,
						"WORK_COMPLETE": true,
						"WORKER_ON": true,
						"WORKER_ONLINE": true,
						"WORKER_OFF": true,
						"WORKER_OFFLINE": true,
						"WORKER_REBOOT": true
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
						e.svg.marker = svg_events_front_layer.append("polygon")
						.attr("class", "work-request")
						.attr("points", "-3,0, 3,0, 0,15");

					}
					else if (e.e == "WORK_COMMAND") {
						events.push(e);

						var y = (id_to_index(e.info.id) + 1) * margin.row + 3;

						e.svg.rect = svg_events_front_layer.append("rect")
						.attr("class", "work-command")
						.attr("y", y)
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
							.style("stroke", "none")
							.style("fill-opacity", 0.8)
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


						// create WORKER_ON event if none has been emitted previously
						var on = false;
						events.forEach(function(f) {
							if (f.e == "WORKER_ON" && f.info["id"] == e.info["id"] && !f.complete) {
								on = true;
							}
						});
						if (!on) {
							events.push({
								e: "WORKER_ON",
								info: { id: e.info.id },
								t: e.t,
								svg: {
									rect: svg_events_back_layer.append("rect")
										.attr("y", (i + 1) * margin.row)
										.attr("height", margin.row)
										.style("stroke", "none")
										.style("fill-opacity", 0.8)
										.style("fill", d3.rgb(colors(i)).brighter(1.8))
								}
							});
						}

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
					else if (e.e == "WORKER_REBOOT") {
						// TODO
					}
				});


				for (var i = 0; i < events.length; i++) {
					var e = events[i];

					// remove old events
					if (e.t < time - past) {

						if (e.e == "WORK_COMMAND" || e.e == "WORKER_ONLINE" || e.e == "WORKER_ON") {
							if (e.complete && e.complete.t < time - past) {
								e.svg.rect.remove();
								events.splice(i, 1);
								i--;
								continue;
							}
						}
						else {
							$.each(e.svg, function(k, v) {
								v.remove();
							});

							events.splice(i, 1);
							i--;
							continue;
						}
					}


					if (e.e == "WORK_REQUEST") {
						e.svg.marker.attr("transform", "translate(" + x(e.t) + ",0)");
					}
					if (e.e == "WORK_COMMAND" || e.e == "WORKER_ONLINE" || e.e == "WORKER_ON") {
						var x1 = x(e.t);
						if (x1 < 0) x1 = -1;
						var x2 = width;
						if (e.complete) x2 = x(e.complete.t);
						e.svg.rect.attr("x", x1).attr("width", x2 - x1);
					}

				}


			}
			setTimeout(poll, update * 1000);
		 }, "json");
	 })();

});
