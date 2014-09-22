$(document).ready(function() {


	var past = 60 * 10;
	var delta = 1; // one sample per second
	var update = 1;
	var data = [];
	var time = -past;
	var tickWidth = 60;

	var width = 600;
	var height = 400;

	var margin = {top: 20, right: 20, bottom: 30, left: 50};


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
//		var s = t % 60;
//		return h+":"+format(m)+":"+format(s)
		return h+":"+format(m)
	});

	var yAxis = d3.svg.axis()
	.scale(y)
	.orient("left");

	var svg = d3.select("body").append("svg")
	.attr("width", width + margin.left + margin.right)
	.attr("height", height + margin.top + margin.bottom);
	var svg_chart = svg.append("g")
	.attr("transform", "translate("+ margin.left +","+ margin.top +")");



	var svg_chart2 = svg.append("g")
	.attr("transform", "translate("+ margin.left +"," + (margin.top + height) +")");

	svg_chart2.append("rect")
	.attr("width", width)
	.attr("height", 10)
	.attr("stroke", "black")
	.attr("fill", "none");





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
	y.domain([0, 3000]);
	svg_xAxis.call(xAxis);
	svg_yAxis.call(yAxis);

	var svg_areas = [];
	var areas = [];


	var first_poll = true;

	(function poll() {
		$.post("poll", { "t": time + delta, "m": past + delta }, function(json) {
			var current = json.current;

			if (first_poll) {
				first_poll = false;

				var colors = d3.scale.category10();

				d3.range(current[0].length - 1).reverse().forEach(function(i) {

					areas[i] = d3.svg.area()
					.x(function(d) { return x(d[0]); })
					.y0(height)
					.y1(function(d) { return y(d3.sum(d.slice(1, 2 + i))); });

					var c = colors(i);

					svg_areas[i] = svg_chart.append("path")
					.attr("class", "area")
					.style("fill", d3.rgb(c).brighter())
					.style("stroke", c)
					.datum(data)
					.attr("d", areas[i]);
				});




			}


			current.forEach(function(d) { data.push(d); });
			while (data.length > past / delta + 1) data.shift();

			time = data[data.length - 1][0];


			x.domain([time - past, time]);
			var l = d3.max([0, Math.ceil((time - past) / tickWidth) * tickWidth]);
			xAxis.tickValues(d3.range(l, time, tickWidth));
			svg_xAxis.call(xAxis);


			for (var i = 0; i < areas.length; i++) {
				svg_areas[i].attr("d", areas[i])
			}

			setTimeout(poll, update * 1000);

		 }, "json");
	 })();

});
