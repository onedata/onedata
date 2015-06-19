from tests.test_common import *
from tests import test_utils

package_dir = os.path.join(os.getcwd(), 'package/deb/binary-amd64')
scripts_dir = os.path.dirname(test_utils.test_file('deb_install_script.py'))

from environment import docker, env


class TestDebInstallation:
    @classmethod
    def setup_class(cls):
        cls.result = env.up(test_utils.test_file('env.json'))

    @classmethod
    def teardown_class(cls):
        docker.remove(cls.result['docker_ids'], force=True, volumes=True)

    # Test if installation has finished successfully
    def test_installation(self):
        gr_node = self.result['gr_nodes'][0]
        (_, _, gr_hostname) = gr_node.partition('@')
        gr_dockername = '_'.join(gr_hostname.split('.')[:2])

        command = 'apt-get update && ' \
                  'apt-get install -y python && ' \
                  'python /root/data/deb_install_script.py'
        command = command.format(package_dir=package_dir)
        assert 0 == docker.run(tty=True,
                               interactive=True,
                               image='debian:sid',
                               hostname='onedata.devel',
                               rm=True,
                               workdir="/root",
                               run_params=['--privileged=true'],
                               link={gr_dockername: 'onedata.org'},
                               volumes=[
                                   (package_dir, '/root/pkg', 'rw'),
                                   (scripts_dir, '/root/data', 'r')
                               ],
                               command=command)
