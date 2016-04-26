#!/bin/sh


#cat pre_"$1".run ../rungengui/test6_90.run > q.run && ./server q.run; ../plot_run

echo '# batman' >> q

for x in `seq 10 10 100`
do
	echo "# $x %" >> q
	cat pre_batman.run ../rungengui/test6_"$x".run > q.run
	./server q.run | awk '/racr-time/{print $2}' >> q
	./server q.run | awk '/racr-time/{print $2}' >> q
	./server q.run | awk '/racr-time/{print $2}' >> q
	./server q.run | awk '/racr-time/{print $2}' >> q
	./server q.run | awk '/racr-time/{print $2}' >> q
done

echo '# robin' >> q

for x in `seq 10 10 100`
do
	echo "# $x %" >> q
	cat pre_robin.run ../rungengui/test6_"$x".run > q.run
	./server q.run | awk '/racr-time/{print $2}' >> q
	./server q.run | awk '/racr-time/{print $2}' >> q
	./server q.run | awk '/racr-time/{print $2}' >> q
	./server q.run | awk '/racr-time/{print $2}' >> q
	./server q.run | awk '/racr-time/{print $2}' >> q
done
