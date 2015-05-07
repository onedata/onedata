from tests import testutil
import json
import socket
import time
import os
import sys

package_dir = os.path.join(os.getcwd(), 'package')
appmock_dir = os.path.join(os.getcwd(), 'appmock')
sys.path.insert(0, appmock_dir)
bamboos_dir = os.path.join(os.getcwd(), 'bamboos', 'docker')
sys.path.insert(0, bamboos_dir)
from appmock import appmock_client
from environment import docker, env


class TestEnvUp:
    @classmethod
    def setup_class(cls):
        command = '''
        import os, shutil, subprocess, sys

        subprocess.call(['apt-key', 'adv', '--keyserver keyserver.ubuntu.com', '--recv-keys', 'D73BB29D', '3A6CFFB3'])
        subprocess.call(['sh', '-c', 'echo "deb http://ppa.launchpad.net/kzemek/boost/ubuntu utopic main" > /etc/apt/sources.list.d/boost.list'])
        subprocess.call(['apt-get', 'update'])

        subprocess.call(['cd', 'pkg'])
        subprocess.call(['dpkg', '-i', 'op-onepanel*.deb', ';', 'apt-get', '-f', '-y', 'install'])
        subprocess.call(['dpkg', '-i', 'op-ccm*.deb', ';', 'apt-get', '-f', '-y', 'install'])
        subprocess.call(['dpkg', '-i', 'oneprovider-node*.deb', ';', 'apt-get', '-f', '-y', 'install'])

        subprocess.call(['service op_onepanel start'])

        sys.exit(ret)
        '''

        cls.result = docker.run(tty=True,
                 interactive=True,
                 image='debian:sid',
                 rm=True,
                 workdir="/root",
                 volumes=[(package_dir, '/root/pkg', 'ro')],
                 command=['python', '-c', command])

    @classmethod
    def teardown_class(cls):
        pass

    # Test if the env_up.py script works as expected.
    def test_results(self):
        pass