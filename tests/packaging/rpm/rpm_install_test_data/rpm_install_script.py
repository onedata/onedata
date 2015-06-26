from subprocess import STDOUT, check_call, check_output, call
import sys

# get packages
packages = check_output(['ls', '/root/pkg']).split()
packages = sorted(packages, reverse=True)
op_worker_package = [path for path in packages
                     if path.startswith('op-worker')
                     and path.endswith('.rpm')][0]
op_ccm_package = [path for path in packages
                  if path.startswith('op-ccm')
                  and path.endswith('.rpm')][0]
op_panel_package = [path for path in packages
                    if path.startswith('op-panel')
                    and path.endswith('.rpm')][0]
oneclient_package = [path for path in packages
                     if path.startswith('oneclient')
                     and not path.startswith('oneclient-debuginfo')
                     and path.endswith('.rpm')][0]

# install dependencies
check_call(['yum', '-y', 'install', 'wget', 'curl'], stderr=STDOUT)

# add onedata repo
check_call(['wget', '-O', '/etc/yum.repos.d/onedata.repo',
            'http://packages.onedata.org/fedora/21/onedata.repo'], stderr=STDOUT)

# add riak repo
check_call(['cp', 'data/basho_riak.repo', '/etc/yum.repos.d/'])

# temporary hack for non existing /run/lock
check_call(['mkdir', '-p', '/run/lock/subsys']) #todo repair non mounting /run/lock

# install all
check_call(['yum', '-y', 'install', 'riak'], stderr=STDOUT)
check_call(['yum', '-y', 'install', '/root/pkg/' + op_panel_package], stderr=STDOUT)
check_call(['yum', '-y', 'install', '/root/pkg/' + op_ccm_package], stderr=STDOUT)
check_call(['yum', '-y', '--enablerepo=onedata', 'install', '/root/pkg/' + op_worker_package], stderr=STDOUT)
check_call(['yum', '-y', '--enablerepo=onedata', 'install', '/root/pkg/' + oneclient_package], stderr=STDOUT)

# package installation validation
check_call(['service', 'op_panel', 'status'])
check_call(['ls', '/etc/op_ccm/app.config'])
check_call(['ls', '/etc/op_worker/app.config'])
check_call(['/usr/bin/oneclient', '--help'])

# oneprovider configure&install
call(['op_panel_admin', '--install', '/root/data/install.cfg'])

# validate oneprovider is running
check_call(['service', 'op_ccm', 'status'])
check_call(['service', 'op_worker', 'status'])

# uninstall
check_call(['op_panel_admin', '--uninstall'])

sys.exit(0)






