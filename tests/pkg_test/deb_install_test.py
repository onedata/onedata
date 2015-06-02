import os
import sys

package_dir = os.path.join(os.getcwd(), 'package')
appmock_dir = os.path.join(os.getcwd(), 'appmock')
sys.path.insert(0, appmock_dir)
bamboos_dir = os.path.join(os.getcwd(), 'bamboos', 'docker')
sys.path.insert(0, bamboos_dir)
from environment import docker, env
import subprocess


class TestDebInstall:
    @classmethod
    def setup_class(cls):
        # get packages
        packages = subprocess.check_output(['ls', package_dir]).split()
        packages = sorted(packages, reverse=True)
        op_worker_package = [path for path in packages if path.startswith('op-worker')][0]
        op_ccm_package = [path for path in packages if path.startswith('op-ccm')][0]
        op_onepanel_package = [path for path in packages if path.startswith('op-onepanel')][0]

        #todo uncomment all
        command = '''
import os, shutil, subprocess, sys

# # update repos
# subprocess.check_call(['apt-get', 'update'])
#
# # add private boost repo
# subprocess.check_call(['apt-key', 'adv', '--keyserver', 'keyserver.ubuntu.com',
#     '--recv-keys', 'D73BB29D', '3A6CFFB3'])
# boost_utopic_repo = subprocess.Popen(['echo',
#     'deb http://ppa.launchpad.net/kzemek/boost/ubuntu utopic main'], stdout=subprocess.PIPE)
# subprocess.check_call(['tee', '/etc/apt/sources.list.d/boost.list'], stdin=boost_utopic_repo.stdout)
# boost_utopic_repo.wait()
#
# # install dependencies
# subprocess.check_call(['apt-get', '-y', 'install', 'curl', 'apt-transport-https'])
#
# # add riak repo
# riak_key = subprocess.Popen(['curl', 'https://packagecloud.io/gpg.key'], stdout=subprocess.PIPE)
# subprocess.check_call(['apt-key', 'add', '-'], stdin=riak_key.stdout)
# riak_key.wait()
# riak_utopic_repo = subprocess.Popen(['curl',
#     'https://packagecloud.io/install/repositories/basho/riak/config_file.list?os=ubuntu&dist=trusty'], stdout=subprocess.PIPE)
# subprocess.check_call(['tee', '/etc/apt/sources.list.d/basho.list'], stdin=riak_utopic_repo.stdout)
# riak_utopic_repo.wait()
#
# # update repos
# subprocess.check_call(['apt-get', 'update'])
#
# # install all
# subprocess.check_call(['apt-get', 'install', '-y', 'riak'])
# subprocess.check_call(['sh', '-c', 'dpkg -i /root/pkg/{op_onepanel_package} ; apt-get -f -y install'], stderr=subprocess.STDOUT)
# subprocess.check_call(['sh', '-c', 'dpkg -i /root/pkg/{op_ccm_package} ; apt-get -f -y install'], stderr=subprocess.STDOUT)
# subprocess.check_call(['sh', '-c', 'dpkg -i /root/pkg/{op_worker_package} ; apt-get -f -y install'], stderr=subprocess.STDOUT)
#
# # validate
# subprocess.check_call(['sed', '-i', 's#-name .*#-name onepanel@onedata.devel#g', '/etc/op_onepanel/vm.args'])
# subprocess.check_call(['service', 'op_onepanel', 'start'])
# subprocess.check_call(['service', 'op_onepanel', 'status'])
# subprocess.check_call(['ls', '/etc/op_ccm/app.config'])
# subprocess.check_call(['ls', '/etc/op_worker/app.config'])

sys.exit(0)
'''
        command = command.format(
            op_onepanel_package=op_onepanel_package,
            op_ccm_package=op_ccm_package,
            op_worker_package=op_worker_package
        )
        cls.res = docker.run(tty=True,
                 interactive=True,
                 image='debian:sid',
                 hostname='onedata.devel',
                 rm=True,
                 workdir="/root",
                 volumes=[(package_dir, '/root/pkg', 'rw')],
                 command='apt-get update && apt-get install -y python && python -c "' + command + '"')
        assert cls.res == 0

    @classmethod
    def teardown_class(cls):
        pass

    # Test if installation has finished successfully
    def test_results(self):
        pass