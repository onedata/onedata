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

# absolute paths
TEST_DIR=$(pwd)/tests
TMP_DIR=${TEST_DIR}/tmp

# paths relative to tests directory
CUCUMBER_DIR=cucumber
FEATURE_DIR=${CUCUMBER_DIR}/features
ENV_DIR=${CUCUMBER_DIR}/environments

# make tmp directory
mkdir -p ${TMP_DIR}

# copy cucumber tests to tmp directory
cp -r  ${TEST_DIR}/${CUCUMBER_DIR} ${TMP_DIR}

# run tests for every env configuration file
for env_file in ${TMP_DIR}/${ENV_DIR}/*.json
do
    env_file_short=$(basename ${env_file})
    # edit env configuration file name in every .feature file
    for feature_file in ${TMP_DIR}/${FEATURE_DIR}/*.feature
    do
        sed -i s/'[a-zA-Z_0-9\-]*.json'/${env_file_short}/g ${feature_file}
    done
    # run tests per given env configuration file
    echo "Running cucumber tests for ${env_file_short}"
    ./test_run.py --test-dir ${TMP_DIR}/${CUCUMBER_DIR}
done

# delete tmp directory
rm -rf ${TMP_DIR}