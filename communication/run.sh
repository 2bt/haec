#!/bin/sh


#cat pre_"$1".run ../rungengui/test6_90.run > q.run && ./server q.run; ../plot_run


for c in 6 5
do
	echo '# gen_config.py' "$c"  >> q

	./gen_config.py $c > config.txt

	for s in batman robin
	do
		echo '#' "$s" >> q

		for x in `seq 0 10 100`
		do
			echo "# $x %" >> q
			cat pre_"$s".run ../rungengui/test6_"$x".run > q.run
			./server q.run | awk '/racr-time/{print $2}' >> q
			./server q.run | awk '/racr-time/{print $2}' >> q
			./server q.run | awk '/racr-time/{print $2}' >> q
			./server q.run | awk '/racr-time/{print $2}' >> q
			./server q.run | awk '/racr-time/{print $2}' >> q
			./server q.run | awk '/racr-time/{print $2}' >> q
			./server q.run | awk '/racr-time/{print $2}' >> q
			./server q.run | awk '/racr-time/{print $2}' >> q
			./server q.run | awk '/racr-time/{print $2}' >> q
			./server q.run | awk '/racr-time/{print $2}' >> q
		done

	done

done
