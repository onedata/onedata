#!/usr/bin/env bash

LOG_DIR=fprof
TEST_DOCKER=profiler
PYTHON_PARSER_SCRIPT=parse.py
SETUP_SCRIPT=setup.py
TEARDOWN_SCRIPT=teardown.py
TESTED_SCRIPT=$1.py
START_PROFILING_SCRIPT=start_fprof.escript
TEST_NAME=$(basename ${TESTED_SCRIPT} .py)
LOG_DIR=${LOG_DIR}/${TEST_NAME}

function docker_name {
    docker ps --filter "name=$1" --format "{{.Names}}" | head -1
}

function docker_ip {
    docker inspect --format '{{ .NetworkSettings.IPAddress }}' $1
}

op_docker=$(docker_name worker)
client_docker=$(docker_name client-host1)
oz_docker=$(docker_name node1)
client_ip=$(docker_ip ${client_docker})
op_node=worker@${op_docker}

TIMESTAMP=$(date +"%F_%T")

./${SETUP_SCRIPT} ${client_ip}

echo "Start profiling"

CASE_LOG_DIR=${LOG_DIR}/${TIMESTAMP}
mkdir -p ${CASE_LOG_DIR}
echo $2 > ${CASE_LOG_DIR}/description
PROFILE_LOG_NAME=fprof.analysis.${TIMESTAMP}
PROFILE_LOG_PATH_DOCKER=/root/bin/node/${PROFILE_LOG_NAME}
PROFILE_LOG=${CASE_LOG_DIR}/${PROFILE_LOG_NAME}

docker exec -it ${TEST_DOCKER} ./${START_PROFILING_SCRIPT} ${op_node} \
    ${PROFILE_LOG_NAME} $(pwd)/${TESTED_SCRIPT} ${client_ip} 1 ${client_docker} ${op_docker} ${oz_docker}

echo "Finished profiling"
docker cp ${op_docker}:${PROFILE_LOG_PATH_DOCKER} .
cp ${PROFILE_LOG_NAME} ${CASE_LOG_DIR}
rm ${PROFILE_LOG_NAME}
echo "Converting ${PROFILE_LOG} to ${CASE_LOG_DIR}/cg_${PROFILE_LOG_NAME}"
erlgrind ${PROFILE_LOG} ${CASE_LOG_DIR}/cg_${PROFILE_LOG_NAME}


./${TEARDOWN_SCRIPT} ${client_ip}

echo "Output files are in directory ${CASE_LOG_DIR}"