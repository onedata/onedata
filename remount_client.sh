#!/usr/bin/env bash

echo REMOUNTING >> dupa
echo REMOUNTING
docker exec $1 su -c \
    "fusermount -u /home/u1/onedata;
    export PROVIDER_HOSTNAME=$2;
    export GLOBAL_REGISTRY_URL=$3;
    /root/bin/oneclient --authentication token \
    --no_check_certificate /home/u1/onedata < /tmp/token" \
    u1