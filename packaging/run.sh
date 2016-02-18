#!/bin/sh

if [ "$ONEPANEL_MULTICAST_ADDRESS" ]; then
	sed -i s/239.255.0.1/"$ONEPANEL_MULTICAST_ADDRESS"/g /etc/op_panel/app.config;
fi

sed -i s/onepanel@127.0.0.1/onepanel@`hostname -f`/g /etc/op_panel/vm.args;
service op_panel start

while true; do sleep 60; done
