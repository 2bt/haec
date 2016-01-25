#!/bin/bash

function reboot() {
	sshpass -p root ssh root@$1 << END
	#reboot
	halt
END
}

function wiki() {
	scp ../index/wiki/dump.txt cubie@$1:/home/cubie/haec/index/wiki
}


#	sshpass -p cubie ssh cubie@$1 << END
#	cd haec/communication/
#	git pull
#	rm worker
#	make worker
#END
#}

for i in 21 22 23 41 42 43; do
	#update 192.168.1.$i &
	#wiki 192.168.1.$i &
	reboot 192.168.1.$i &
done
