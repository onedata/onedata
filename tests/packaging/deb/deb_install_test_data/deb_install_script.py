from subprocess import Popen, PIPE, STDOUT, check_call, check_output
import sys

# get packages
packages = check_output(['ls', '/root/pkg']).split()
packages = sorted(packages, reverse=True)
op_worker_package = \
    [path for path in packages if path.startswith('op-worker')][0]
op_ccm_package = \
    [path for path in packages if path.startswith('op-ccm')][0]
op_panel_package = \
    [path for path in packages if path.startswith('op-panel')][0]
oneclient_package = [path for path in packages
                     if path.startswith('oneclient')
                     and not path.startswith('oneclient-debuginfo')][0]
# update repositories
check_call(['apt-get', 'update'])

# add private boost&tbb repository
check_call(['apt-key', 'adv', '--keyserver', 'keyserver.ubuntu.com',
            '--recv-keys', 'D73BB29D', '3A6CFFB3'])
boost_utopic_repo = Popen(['echo', 'deb http://ppa.launchpad.net/kzemek/boost/'
                                   'ubuntu utopic main'], stdout=PIPE)
check_call(['tee', '/etc/apt/sources.list.d/boost.list'],
           stdin=boost_utopic_repo.stdout)
boost_utopic_repo.wait()
tbb_utopic_repo = Popen(['echo', 'deb http://ppa.launchpad.net/kzemek/intel-tbb/'
                                   'ubuntu utopic main'], stdout=PIPE)
check_call(['tee', '/etc/apt/sources.list.d/intel-tbb.list'],
           stdin=tbb_utopic_repo.stdout)
tbb_utopic_repo.wait()
onedata_testing_repo = Popen(['echo', 'deb http://packages.onedata.org/'
                                   'debian testing main'], stdout=PIPE)
check_call(['tee', '/etc/apt/sources.list.d/onedata.list'],
           stdin=onedata_testing_repo.stdout)
onedata_testing_repo.wait()

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

# update repositories
check_call(['apt-get', 'update'])

# install all
check_call(['apt-get', 'install', '-y', 'couchbase-server'])
check_call(['sh', '-c', 'dpkg -i /root/pkg/{package} ; apt-get -f -y '
                        'install'.format(package=op_panel_package)
            ], stderr=STDOUT)
check_call(['sh', '-c', 'dpkg -i /root/pkg/{package} ; apt-get -f -y '
                        'install'.format(package=op_ccm_package)
            ], stderr=STDOUT)
check_call(['sh', '-c', 'dpkg -i /root/pkg/{package} ; apt-get -f -y '
                        'install'.format(package=op_worker_package)
            ], stderr=STDOUT)
check_call(['sh', '-c', 'dpkg -i /root/pkg/{package} ; apt-get -f -y '
                        'install'.format(package=oneclient_package)
            ], stderr=STDOUT)

# package installation validation
check_call(['service', 'op_panel', 'status'])
check_call(['ls', '/etc/op_ccm/app.config'])
check_call(['ls', '/etc/op_worker/app.config'])
check_call(['/usr/bin/oneclient', '--help'])

# oneprovider configure&install
check_call(['op_panel_admin', '--install', 'data/install.cfg'])

# validate oneprovider is running
check_call(['service', 'op_ccm', 'status'])
check_call(['service', 'op_worker', 'status'])

# uninstall
check_call(['op_panel_admin', '--uninstall'])

sys.exit(0)
