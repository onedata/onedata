#!/bin/bash

CEPH_STOP_TIMEOUT=60

function stop_service {
  echo -e "Stopping ${2:-$1}..."
  service "$1" stop >/dev/null 2>&1
}

function wait_for_process() {
  while pgrep "$1" >/dev/null; do
    sleep 0.5;
  done
}

function stop_with_timeout {
  export -f wait_for_process # make function available in timeout invocation

  if pgrep "$1" >/dev/null; then
    echo -e "Stopping ${2:-$1}..."
    pkill -TERM -e "$1"

    timeout $CEPH_STOP_TIMEOUT bash -c "wait_for_process $1"
    pkill -KILL -e "$1"
  fi
}

function stop_oneprovider {
  echo -e "\nGracefully stopping oneprovider...\n"

  stop_service op_panel
  stop_service op_worker
  stop_service cluster_manager
  stop_service couchbase-server couchbase
  stop_with_timeout ceph-osd "ceph_osd"
  stop_with_timeout ceph-mgr "ceph_mgr"
  stop_with_timeout ceph-mon "ceph_mon"

  echo -e "\nAll services stopped. Exiting..."

  exit 0
}

trap stop_oneprovider SIGHUP SIGINT SIGTERM

/root/oneprovider.py &
wait $!
