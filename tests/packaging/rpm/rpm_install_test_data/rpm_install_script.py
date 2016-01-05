from __future__ import print_function
from subprocess import STDOUT, check_call, check_output, call
import sys

# get packages
packages = check_output(['ls', '/root/pkg']).split()
packages = sorted(packages, reverse=True)
op_worker_package = [path for path in packages
                     if path.startswith('op-worker')
                     and path.endswith('.rpm')][0]
cluster_manager_package = [path for path in packages
                  if path.startswith('cluster-manager')
                  and path.endswith('.rpm')][0]
op_panel_package = [path for path in packages
                    if path.startswith('op-panel')
                    and path.endswith('.rpm')][0]
oneprovider_package = [path for path in packages
                    if path.startswith('oneprovider')
                    and path.endswith('.rpm')][0]

# Reinstall glibc-common to fix locales
check_call(['yum', '-y', 'reinstall', 'glibc-common'])

# install dependencies
check_call(['yum', '-y', 'install', 'wget', 'curl'], stderr=STDOUT)

# add onedata repo
check_call(['wget', '-O', '/etc/yum.repos.d/onedata.repo',
            'http://packages.onedata.org/fedora/21/onedata.repo'], stderr=STDOUT)

# get couchbase
check_call(['wget', 'http://packages.couchbase.com/releases/4.0.0/couchbase-server-community-4.0.0-centos7.x86_64.rpm'])

# temporary hack for non existing /run/lock
check_call(['mkdir', '-p', '/run/lock/subsys']) #todo repair non mounting /run/lock

# install all
check_call(['yum', '-y', 'install', 'couchbase-server-community-4.0.0-centos7.x86_64.rpm'], stderr=STDOUT)
check_call(['yum', '-y', 'install', '/root/pkg/' + op_panel_package], stderr=STDOUT)
check_call(['yum', '-y', 'install', '/root/pkg/' + cluster_manager_package], stderr=STDOUT)
check_call(['yum', '-y', '--enablerepo=onedata', 'install', '/root/pkg/' + op_worker_package], stderr=STDOUT)
check_call(['yum', '-y', 'install', '/root/pkg/' + oneprovider_package], stderr=STDOUT)

# package installation validation
check_call(['service', 'op_panel', 'status'])
check_call(['ls', '/etc/cluster_manager/app.config'])
check_call(['ls', '/etc/op_worker/app.config'])

# fix missing bundle
check_call(['touch', '/etc/ssl/cert.pem']) #todo do something with it

# oneprovider configure&install
check_call(['op_panel_admin', '--install', '/root/data/install.cfg'])

# validate oneprovider is running
check_call(['service', 'cluster_manager', 'status'])
check_call(['service', 'op_worker', 'status'])

# uninstall
# check_call(['op_panel_admin', '--uninstall']) #todo fix stopping application

sys.exit(0)