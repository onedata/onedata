from tests import *
import tests.utils.path_utils

package_dir = os.path.join(os.getcwd(), 'package/fedora-23-x86_64/x86_64')
scripts_dir = os.path.dirname(
    tests.utils.path_utils.config_file('rpm_install_script.py'))

from environment import docker, env


class TestRpmInstallation:
    @classmethod
    def setup_class(cls):
        cls.result = env.up(tests.utils.path_utils.config_file('env.json'))

    @classmethod
    def teardown_class(cls):
        docker.remove(cls.result['docker_ids'], force=True, volumes=True)

    # Test if installation has finished successfully
    def test_installation(self):
        oz_node = self.result['oz_worker_nodes'][0]
        (_, _, oz_dockername) = oz_node.partition('@')

        command = 'dnf install -y python && ' \
                  'python /root/data/rpm_install_script.py'

        container = docker.run(tty=True,
                               interactive=True,
                               detach=True,
                               image='onedata/fedora-systemd:23',
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
