from tests import test_utils
from tests.test_common import *

package_dir = os.path.join(os.getcwd(), 'package/fedora-23-x86_64/x86_64')
scripts_dir = os.path.dirname(test_utils.test_file('rpm_install_script.py'))

from environment import docker, env
import sys


class TestRpmInstallation:
    @classmethod
    def setup_class(cls):
        cls.result = env.up(test_utils.test_file('env.json'))

    @classmethod
    def teardown_class(cls):
        docker.remove(cls.result['docker_ids'], force=True, volumes=True)

    # Test if installation has finished successfully
    def test_installation(self):
        gr_node = self.result['gr_nodes'][0]
        (_, _, gr_dockername) = gr_node.partition('@')

        command = 'dnf install -y python && ' \
                  'python /root/data/rpm_install_script.py'

        container = docker.run(tty=True,
                               interactive=True,
                               detach=True,
                               image='onedata/fedora-systemd:22',
                               hostname='devel.localhost.local',
                               workdir="/root",
                               run_params=['--privileged=true'],
                               link={gr_dockername: 'onedata.org'},
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
