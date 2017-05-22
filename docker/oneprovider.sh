#!/bin/bash

function stop_service {
  echo -e "Stopping ${2:-$1}..."
  service $1 stop >/dev/null 2>&1
}

function stop_oneprovider {
  echo -e "\nGracefully stopping oneprovider...\n"

  stop_service op_panel
  stop_service op_worker
  stop_service cluster_manager
  stop_service couchbase-server couchbase

  echo -e "\nAll services stopped. Exiting..."

  exit 0
}

trap stop_oneprovider SIGHUP SIGINT SIGTERM

/root/oneprovider.py &
wait $!
