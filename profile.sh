#!/usr/bin/env bash

TIMESTAMP=$(date +"%T")
PROFILE_LOG=profiling_logs.${TIMESTAMP}
PROFILING_SUMMARY=summary.${TIMESTAMP}
PROFILE_LOG_PATH=/root/bin/node/${PROFILE_LOG}
TEST_DOCKER=profiler
PYTHON_PARSER_SCRIPT=parse.py
#SETUP_SCRIPT=setup.py
#TEARDOWN_SCRIPT=teardown.py
TESTED_SCRIPT=profiled_test.py


function docker_name {
    docker ps --filter "name=$1" --format "{{.Names}}" | head -1
}

function docker_ip {
    docker inspect --format '{{ .NetworkSettings.IPAddress }}' $1
}

function wait_key {
    read -rsp $'\nPress any key to continue...\n\n' -n1 key
}

op_docker=$(docker_name worker)
client_docker=$(docker_name client-host1)
client_ip=$(docker_ip ${client_docker})
op_node=worker@${op_docker}

docker exec -it ${TEST_DOCKER} ./start_eprof.escript ${op_node} \
    ${PROFILE_LOG} $(pwd)/${TESTED_SCRIPT} ${client_ip}

docker cp ${op_docker}:${PROFILE_LOG_PATH} .

docker exec -it ${TEST_DOCKER} ./parse.py ${op_node} ${PROFILE_LOG} ${PROFILING_SUMMARY}

#echo $out
#
#./find_name.escript < parsed_output

#