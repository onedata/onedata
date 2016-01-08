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
# oneclient_package = [path for path in packages %todo build client package
#                      if path.startswith('oneclient')
#                      and not path.startswith('oneclient-debuginfo')][0]
oneprovider_package = [path for path in packages
                       if path.startswith('oneprovider')][0]

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
check_call(['apt-get', '-y', 'install', 'curl', 'apt-transport-https', 'wget'])

# add our own repo
onedata_key = Popen(['curl', 'http://packages.onedata.org/GPG-KEY-onedata'], stdout=PIPE)
check_call(['apt-key', 'add', '-'], stdin=onedata_key.stdout)
onedata_key.wait()
with open('/etc/apt/sources.list.d/onedata.list', 'w') as f:
  print("deb http://packages.onedata.org/debian/ testing main", file=f)

# update repositories
check_call(['apt-get', 'update'])

# get couchbase
check_call(['wget', 'http://packages.couchbase.com/releases/4.0.0/couchbase-server-community_4.0.0-ubuntu14.04_amd64.deb'])

# install
check_call(['sh', '-c', 'dpkg -i couchbase-server-community_4.0.0-ubuntu14.04_amd64.deb '
                        '; apt-get -f -y install'
            ], stderr=STDOUT)
check_call(['sh', '-c', 'dpkg -i /root/pkg/{package} ; apt-get -f -y '
                        'install'.format(package=op_panel_package)
            ], stderr=STDOUT)
check_call(['sh', '-c', 'dpkg -i /root/pkg/{package} ; apt-get -f -y '
                        'install'.format(package=cluster_manager_package)
            ], stderr=STDOUT)
check_call(['sh', '-c', 'dpkg -i /root/pkg/{package} ; apt-get -f -y '
                        'install'.format(package=op_worker_package)
            ], stderr=STDOUT)
# check_call(['sh', '-c', 'dpkg -i /root/pkg/{package} ; apt-get -f -y ' %todo build client package
#                         'install'.format(package=oneclient_package)
#             ], stderr=STDOUT)
check_call(['dpkg', '-i', '/root/pkg/{package}'.format(package=oneprovider_package)], stderr=STDOUT)

# package installation validation
check_call(['service', 'op_panel', 'status'])
check_call(['ls', '/etc/cluster_manager/app.config'])
check_call(['ls', '/etc/op_worker/app.config'])
# check_call(['/usr/bin/oneclient', '--help']) %todo build client package

# disable gr cert verification
check_call(['sed', '-i', 's/\'{verify_gr_cert, true}\'/\'{verify_gr_cert, false}\'/g', '/etc/op_panel/app.config'])
check_call(['service', 'op_panel', 'restart'])

# fix missing bundle
check_call(['touch', '/etc/ssl/cert.pem']) #todo do something with it

# oneprovider configure&install
check_call(['op_panel_admin', '--install', 'data/install.cfg'])

# validate oneprovider is running
check_call(['service', 'cluster_manager', 'status'])
check_call(['service', 'op_worker', 'status'])

# uninstall
# check_call(['op_panel_admin', '--uninstall'])  #todo fix stopping application

sys.exit(0)