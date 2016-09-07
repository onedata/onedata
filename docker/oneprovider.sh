#!/bin/bash
# chkconfig: 2345 20 80
# description: oneprovider service

# Source function library.
. /etc/init.d/functions

start() {
    while read -d $'\0' ENV; do export "$ENV"; done < /proc/1/environ
    /root/oneprovider.py &
}

case "$1" in
    start)
       start
       ;;
    *)
       echo "Usage: $0 start"
esac

exit 0