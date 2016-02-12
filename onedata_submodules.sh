#!/usr/bin/env bash

#####################################################################
# @author Michal Wrzeszcz
# @copyright (C): 2016 ACK CYFRONET AGH
# This software is released under the MIT license
# cited in 'LICENSE.txt'.
#####################################################################
# This script should be used instead of git submodule command.
#####################################################################

# find git url
GIT_URL=$(git config --get remote.origin.url | sed -e 's/\(\/[^/]*\)$//g')
GIT_URL=$(if [ "${GIT_URL}" = "file:/" ]; then echo 'ssh://git@git.plgrid.pl:7999/vfs'; else echo ${GIT_URL}; fi)
ONEDATA_GIT_URL=$(if [ "${ONEDATA_GIT_URL}" = "" ]; then echo ${GIT_URL}; else echo ${ONEDATA_GIT_URL}; fi)

sed -i.bak "s#ONEDATA_GIT_URL#${ONEDATA_GIT_URL}#g" .gitmodules

git submodule $@

rm -f .gitmodules
mv .gitmodules.bak .gitmodules
