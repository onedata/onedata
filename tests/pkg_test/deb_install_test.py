import os
import sys

package_dir = os.path.join(os.getcwd(), 'package')
appmock_dir = os.path.join(os.getcwd(), 'appmock')
sys.path.insert(0, appmock_dir)
bamboos_dir = os.path.join(os.getcwd(), 'bamboos', 'docker')
sys.path.insert(0, bamboos_dir)
from environment import docker, env


class TestDebInstall:
    @classmethod
    def setup_class(cls):
        command = '''
import os, shutil, subprocess, sys

subprocess.call(['apt-key', 'adv', '--keyserver', 'keyserver.ubuntu.com', '--recv-keys', 'D73BB29D', '3A6CFFB3'])
subprocess.call(['echo', '"deb http://ppa.launchpad.net/kzemek/boost/ubuntu utopic main" > /etc/apt/sources.list.d/boost.list'])
# subprocess.call(['apt-get', '-y', 'install', 'curl', 'apt-transport-https'])
# subprocess.call(['curl', 'https://packagecloud.io/gpg.key | apt-key add -'])
# subprocess.call(['curl', '"https://packagecloud.io/install/repositories/basho/riak/config_file.list?os=ubuntu&dist=trusty" > /etc/apt/sources.list.d/basho.list'])
subprocess.call(['apt-get', 'update'])


subprocess.call(['cd', '/root/pkg'])
subprocess.call(['dpkg', '-i', 'op-onepanel*.deb'])
subprocess.call(['apt-get', '-f', '-y', 'install'])
subprocess.call(['dpkg', '-i', 'op-ccm*.deb'])
subprocess.call(['apt-get', '-f', '-y', 'install'])
subprocess.call(['dpkg', '-i', 'oneprovider-node*.deb'])
subprocess.call(['apt-get', '-f', '-y', 'install'])
subprocess.call(['apt-get', ' install', ' riak'])

subprocess.call(['sed', '-i', 's#-name .*#-name onepanel@onedata.devel#g', '/etc/op_onepanel/vm.args'])
ret1 = subprocess.call(['service', 'op_onepanel', 'start'])
ret2 = subprocess.call(['ls', '/etc/op_ccm/app.config'])
ret3 = subprocess.call(['ls', '/etc/oneprovider_node/app.config'])

sys.exit(ret1 + ret2 + ret3)
'''

        cls.result = docker.run(tty=True,
                 interactive=True,
                 image='onedata/worker',
                 hostname='onedata.devel',
                 rm=True,
                 workdir="/root",
                 volumes=[(package_dir, '/root/pkg', 'rw')],
                 command=['python', '-c', command])

    @classmethod
    def teardown_class(cls):
        pass

    # Test if installation has finished successfully
    def test_results(self):
        assert self.result == 0