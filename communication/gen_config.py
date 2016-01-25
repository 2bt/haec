#!/usr/bin/python



workers = []
switches = []


counter = 0
def get_new_id():
	global counter
	i = counter % 8 + 1 + (counter / 8 + 1) * 1000
	counter += 1
	return i


def switch(depth, parent_id=0):
	if depth == 0: return
	my_id = get_new_id()
	switches.append("switch %d %d" % (my_id, parent_id))

	for _ in range(5): workers.append("worker %d %d 0.0.0.0 CUBIEBOARD" % (get_new_id(), my_id))
	for _ in range(2): switch(depth - 1, my_id)



switch(2)

print "\n".join(switches)
print
print "\n".join(workers)
