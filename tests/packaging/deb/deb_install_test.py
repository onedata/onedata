from tests import test_utils
from tests.test_common import *

package_dir = os.path.join(os.getcwd(), 'package/wily/binary-amd64')
scripts_dir = os.path.dirname(test_utils.test_file('deb_install_script.py'))

from environment import docker, env
import sys


class TestDebInstallation:
    @classmethod
    def setup_class(cls):
        cls.result = env.up(test_utils.test_file('env.json'))

    @classmethod
    def teardown_class(cls):
        docker.remove(cls.result['docker_ids'], force=True, volumes=True)

    # Test if installation has finished successfully
    def test_installation(self):
        oz_node = self.result['oz_worker_nodes'][0]
        (_, _, oz_dockername) = oz_node.partition('@')

        command = 'apt-get update && ' \
                  'apt-get install -y python && ' \
                  'python /root/data/deb_install_script.py'

        container = docker.run(tty=True,
                               interactive=True,
                               detach=True,
                               image='ubuntu:15.10',
                               hostname='devel.localhost.local',
                               workdir="/root",
                               run_params=['--privileged=true'],
                               link={oz_dockername: 'onedata.org'},
                               stdin=sys.stdin,
                               stdout=sys.stdout,
                               stderr=sys.stderr,
                               volumes=[
                                   (package_dir, '/root/pkg', 'ro'),
                                   (scripts_dir, '/root/data', 'ro')
                               ],
                               reflect=[('/sys/fs/cgroup', 'rw')])

        try:
            assert 0 == docker.exec_(container,
                                     command=command,
                                     interactive=True,
                                     tty=True)
        finally:
            docker.remove([container], force=True, volumes=True)
