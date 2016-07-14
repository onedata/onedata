#!/usr/bin/env bash

LOG_DIR=profiling
TEST_DOCKER=profiler
PYTHON_PARSER_SCRIPT=parse.py
SETUP_SCRIPT=setup.py
TEARDOWN_SCRIPT=teardown.py
TESTED_SCRIPT=profiled_test.py
START_EPROF=start_eprof.escript

mkdir -p ${LOG_DIR}

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
oz_docker=$(docker_name node1)
client_ip=$(docker_ip ${client_docker})
op_node=worker@${op_docker}


TIMESTAMP=$(date +"%T")

for range in 1 10 50 100 150 200 500 1000
do
echo "Starting profiling for ${range}"

./${SETUP_SCRIPT} ${client_ip} ${range}

CASE_LOG_DIR=${LOG_DIR}/dirs_${range}
mkdir -p ${CASE_LOG_DIR}
PROFILE_LOG_NAME=eprof_log.${TIMESTAMP}
PROFILE_LOG_PATH_DOCKER=/root/bin/node/${PROFILE_LOG_NAME}
PROFILE_LOG=${CASE_LOG_DIR}/${PROFILE_LOG_NAME}
PROFILE_SUMMARY=${CASE_LOG_DIR}/summary.${TIMESTAMP}

echo "Finished setup for ${range}"
docker exec -it ${TEST_DOCKER} ./${START_EPROF} ${op_node} \
    ${PROFILE_LOG_NAME} $(pwd)/${TESTED_SCRIPT} ${client_ip} ${range} ${client_docker} ${op_docker} ${oz_docker}

docker cp ${op_docker}:${PROFILE_LOG_PATH_DOCKER} ${CASE_LOG_DIR}
docker exec -it ${TEST_DOCKER} ./${PYTHON_PARSER_SCRIPT} ${op_node} ${PROFILE_LOG} ${PROFILE_SUMMARY}

./${TEARDOWN_SCRIPT} ${client_ip}
echo "Finished profiling for ${range}"
done