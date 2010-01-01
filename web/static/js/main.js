$(document).ready(function() {

	$("form").submit(function() {
		var p = $(this).parent();
		p.css("background-color", "red");

		var q = $(this).serialize();
		$("input[name=cmd]", this).val("");

		$.post("command", q, function(ret) {
			p.css("background-color", "");
			console.log(ret);
		});
		return false;
	});









	var past = 60 * 5;
	var tickWidth = 60;

	var delta = 1; // one sample per second
	var update = 1;
	var current = [];
	var events = [];
	var time = -past;
	var width = 600;
	var height = 400;
	var margin = { top: 20, right: 20, bottom: 0, left: 50, middle: 24, row: 25 };


	var x = d3.scale.linear()
	.range([0, width]);

	var y = d3.scale.linear()
	.range([height, 0]);


	var format = d3.format("02d");
	var xAxis = d3.svg.axis()
	.scale(x)
	.orient("bottom")
	.tickFormat(function (t) {
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
	.text("Current (mA)");


	x.domain([-past, 0]);
	y.domain([0, 3500]);
	svg_xAxis.call(xAxis);
	svg_yAxis.call(yAxis);

	var svg_areas = [];
	var areas = [];

	var colors = d3.scale.category10();

	var first_poll = true;

	(function poll() {
		$.post("poll", { "t": time, "m": past + delta }, function(json) {
			console.log("poll", json);

			if (json.current.length > 0) {
				if (first_poll) {
					first_poll = false;



					svg.attr("height", height + margin.top + margin.bottom + margin.middle +
						(json.current[0].length + 1) * margin.row
					);

					d3.range(json.current[0].length - 1).forEach(function(i) {

						areas[i] = d3.svg.area()
						.x(function(d) { return x(d[0]); })
						.y0(height)
						.y1(function(d) { return y(d3.sum(d.slice(i+1))); });

						var c = colors(i);

						svg_areas[i] = svg_chart.append("path")
						.attr("class", "area")
						.style("fill", d3.rgb(c).brighter())
						.style("stroke", c)
						.datum(current)
						.attr("d", areas[i]);

						// lanes
						svg_events.append("rect")
						.attr("width", width)
						.attr("height", margin.row)
						.attr("y", (i + 1) * margin.row)
						.style("fill", d3.rgb(c).brighter())
						.style("fill-opacity", 0.5);
					});

				}


				json.current.forEach(function(d) { current.push(d); });
				while (current.length > past / delta + 1) current.shift();
				time = current[current.length - 1][0];


				x.domain([time - past, time]);
				var l = d3.max([0, Math.ceil((time - past) / tickWidth) * tickWidth]);
				xAxis.tickValues(d3.range(l, time, tickWidth));
				svg_xAxis.call(xAxis);


				for (var i = 0; i < areas.length; i++) {
					svg_areas[i].attr("d", areas[i])
				}



				// events
				json.events.forEach(function(e, i) {
					console.log(e.e);
					if (!{
						"WORK_REQUEST": true,
						"WORK_COMMAND": true,
						"WORK_COMPLETE": true
					}[e.e]) return;


					// construct info
					e.info = {}
					e.d.split("; ").forEach(function(l) {
						l = l.split(": ")
						e.info[l[0]] = l[1];
					});

					if (e.e == "WORK_REQUEST") {
						events.push(e);

						e.circle = svg_events.append("circle")
						.attr("class", "work-request")
						.attr("r", margin.row / 2 - 2)
						.attr("cy", margin.row / 2);


					}
					if (e.e == "WORK_COMMAND") {
						events.push(e);

						var d = e.info.id % 1000 - 1;
						var i = (e.info.id - d - 1) / 1000 - 1;
						var y = (d + i * 8 + 1) * margin.row + 5;

						e.rect = svg_events.append("rect")
						.attr("class", "work-command")
						.attr("y", y)
						.attr("height", margin.row - 10);

						events.forEach(function(f) {
							if (f.e == "WORK_REQUEST" &&
								f.info["work-id"] == e.info["work-id"]) {
								e.request = f;
								f.command = e;
								return;
							}
						});
					}
					else if (e.e == "WORK_COMPLETE") {
						events.forEach(function(f) {
							if (f.e == "WORK_COMMAND" &&
								f.info["work-id"] == e.info["work-id"]) {
								e.command = f;
								f.complete = e;
								return;
							}
						});
					}

				});


				for (var i = 0; i < events.length; i++) {
					var e = events[i];

					// remove old events
					if (e.t < time - past) {
						if (e.e == "WORK_REQUEST") {
							e.circle.remove();
						}

						if (e.e == "WORK_COMMAND") {
							if (e.complete && e.complete.t < time - past) {
								e.rect.remove();
								events.splice(i, 1);
								i--;
								continue;
							}
						}
						else {
							events.splice(i, 1);
							i--;
							continue;
						}
					}


					if (e.e == "WORK_REQUEST") {
						e.circle.attr("cx", x(e.t));
					}
					if (e.e == "WORK_COMMAND") {
						var w = width;
						if (e.complete) w = d3.min([w, x(e.complete.t) - x(e.t)]);
						e.rect.attr("x", x(e.t))
						.attr("width", w);
					}

				}


			}
			setTimeout(poll, update * 1000);
		 }, "json");
	 })();

});
