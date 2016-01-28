#!/usr/bin/python


import sys

depth = int(sys.argv[1]) if len(sys.argv) > 1 else 2



lines = []
switches_left = []

counter = 0
def get_new_id():
	global counter
	i = counter % 8 + 1 + (counter / 8 + 1) * 1000
	counter += 1
	return i

def new_switch(parent_id, depth):
	if depth == 0: return
	new_id = get_new_id()
	lines.append("switch %d %d" % (new_id, parent_id))
	switches_left.append((new_id, depth))


def expand_switch(parent_id, depth):
	for _ in range(5):
		lines.append("worker %d %d 0.0.0.0 CUBIEBOARD" % (get_new_id(), parent_id))
	for _ in range(2):
		new_switch(parent_id, depth - 1)

new_switch(0, depth)

while switches_left:
	parent_id, depth = switches_left.pop(0)
	expand_switch(parent_id, depth)


print "\n".join(lines)
