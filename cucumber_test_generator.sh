#!/usr/bin/env bash

#####################################################################
# @author Jakub Kudzia
# @copyright (C): 2016 ACK CYFRONET AGH
# This software is released under the MIT license
# cited in 'LICENSE.txt'.
#####################################################################
# This script starts cucumber tests with different environment
# configurations.
#####################################################################

#TODO hardcoded envs???
ENVS=("env.json" "env2.json")

# absolute paths
TEST_DIR=$(pwd)/tests
TMP_DIR=${TEST_DIR}/tmp

# paths relative to tests directory
CUCUMBER_DIR=cucumber
FEATURE_DIR=${CUCUMBER_DIR}/features

# make tmp directory
mkdir -p ${TMP_DIR}

# copy cucumber tests to tmp directory
cp -r  ${TEST_DIR}/${CUCUMBER_DIR} ${TMP_DIR}

# run tests for every env configuration file
for env_file in ${ENVS[*]}
do
    # edit env configuration file name in every feature
    for feature_file in ${TMP_DIR}/${FEATURE_DIR}/*.feature
    do
        echo ${feature_file}
        # TODO edit env name in all .feature files
        # ...
    done
    # run tests per given env configuration file
#    ./test_run.py --test-dir ${TMP_DIR}
done

#TODO  move logs to log directory
# chyba nie trzeba bedzie, logi sa tam od razu, trzeba tylko dodac
# podkatalog z nazwa enva


# delete tmp directory
rm -rf ${TMP_DIR}