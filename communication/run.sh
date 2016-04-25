#!/bin/sh

if [ ! $1 ]
then
	echo "usage:"
	echo "$0 batman"
	echo "$0 robin"
	echo "$0 allin"
	exit
fi


cat pre_"$1".run ../rungengui/test6.run > q.run && ./server q.run; ../plot_run
