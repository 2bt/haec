
function id_to_index(id) {
	var d = id % 1000 - 1;
	var i = (id - d - 1) / 1000 - 1;
	return d + i * 8;
}

var margin = { top: 20, right: 20, bottom: 20, left: 50, middle: 24, row: 15, left_label:45 };
var lane_titles = ["Cubie 1", "Cubie 2", "Cubie 3", "Cubie 4", "Cubie 5", "Cubie 6", "Master", "Switch"];

var colors = d3.scale.category10();

