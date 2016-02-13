#!/bin/sh

if [ ! $1 ]
then
	echo "usage:"
	echo "$0 batman"
	echo "$0 robin"
	exit
fi


cat pre_"$1".run ../rungengui/test.run > q.run && ./server q.run && ../plot_run
