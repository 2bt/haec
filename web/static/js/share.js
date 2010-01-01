
function id_to_index(id) {
	var d = id % 1000 - 1;
	var i = (id - d - 1) / 1000 - 1;
	return d + i * 8;
}
var tree_margin = 40;
var margin = { top: 20, right: 20, bottom: 20, left: 50+tree_margin, middle: 24, row: 15, left_label:45+tree_margin };
var lane_titles = ["Switch 1", "Cubie 1", "Cubie 2", "Cubie 3", "Switch 2", "Cubie 4", "Cubie 5", "Cubie 6"];

var colors = d3.scale.category10();

