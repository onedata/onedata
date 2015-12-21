from __future__ import print_function
from subprocess import Popen, PIPE, STDOUT, check_call, check_output
import sys

# get packages
packages = check_output(['ls', '/root/pkg']).split()
packages = sorted(packages, reverse=True)
op_worker_package = \
    [path for path in packages if path.startswith('op-worker')][0]
cluster_manager_package = \
    [path for path in packages if path.startswith('cluster-manager')][0]
op_panel_package = \
    [path for path in packages if path.startswith('op-panel')][0]
oneclient_package = [path for path in packages
                     if path.startswith('oneclient')
                     and not path.startswith('oneclient-debuginfo')][0]
# update repositories
check_call(['apt-get', 'update'])

# add locale
check_call(['locale-gen', 'en_US.UTF-8'])

# add private boost repository
check_call(['apt-key', 'adv', '--keyserver', 'keyserver.ubuntu.com',
            '--recv-keys', '3A6CFFB3'])
boost_utopic_repo = Popen(['echo', 'deb http://ppa.launchpad.net/kzemek/boost/'
                                   'ubuntu vivid main'], stdout=PIPE)
check_call(['tee', '/etc/apt/sources.list.d/boost.list'],
           stdin=boost_utopic_repo.stdout)
boost_utopic_repo.wait()

# install dependencies
check_call(['apt-get', '-y', 'install', 'curl', 'apt-transport-https'])

# add riak repo
riak_key = Popen(['curl', 'https://packagecloud.io/gpg.key'], stdout=PIPE)
check_call(['apt-key', 'add', '-'], stdin=riak_key.stdout)
riak_key.wait()
riak_utopic_repo = Popen(['curl', 'https://packagecloud.io/install/repositories'
                                  '/basho/riak/config_file.list?os=ubuntu&dist='
                                  'trusty'], stdout=PIPE)
check_call(['tee', '/etc/apt/sources.list.d/basho.list'],
           stdin=riak_utopic_repo.stdout)
riak_utopic_repo.wait()

# add our own repo
onedata_key = Popen(['curl', 'http://packages.onedata.org/GPG-KEY-onedata'], stdout=PIPE)
check_call(['apt-key', 'add', '-'], stdin=onedata_key.stdout)
onedata_key.wait()
with open('/etc/apt/sources.list.d/onedata.list', 'w') as f:
  print("deb http://packages.onedata.org/debian/ testing main", file=f)

# update repositories
check_call(['apt-get', 'update'])

check_call(['apt-get', 'install', 'couchbase-server'])
check_call(['sh', '-c', 'dpkg -i /root/pkg/{package} ; apt-get -f -y '
                        'install'.format(package=op_panel_package)
            ], stderr=STDOUT)
check_call(['sh', '-c', 'dpkg -i /root/pkg/{package} ; apt-get -f -y '
                        'install'.format(package=cluster_manager_package)
            ], stderr=STDOUT)
check_call(['sh', '-c', 'dpkg -i /root/pkg/{package} ; apt-get -f -y '
                        'install'.format(package=op_worker_package)
            ], stderr=STDOUT)
check_call(['sh', '-c', 'dpkg -i /root/pkg/{package} ; apt-get -f -y '
                        'install'.format(package=oneclient_package)
            ], stderr=STDOUT)

# package installation validation
check_call(['service', 'op_panel', 'status'])
check_call(['ls', '/etc/cluster_manager/app.config'])
check_call(['ls', '/etc/op_worker/app.config'])
check_call(['/usr/bin/oneclient', '--help'])

# oneprovider configure&install
check_call(['op_panel_admin', '--install', 'data/install.cfg'])

# validate oneprovider is running
check_call(['service', 'cluster_manager', 'status'])
check_call(['service', 'op_worker', 'status'])

# uninstall
check_call(['op_panel_admin', '--uninstall'])

sys.exit(0)
