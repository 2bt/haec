#!/bin/bash

function update() {
	sshpass -p cubie ssh cubie@$1 << END
	cd haec/communication/
	git pull
	make worker
END
}

for i in $(seq 21 27); do
	update 192.168.1.$i &
done
