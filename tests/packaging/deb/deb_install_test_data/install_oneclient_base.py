import sys
from subprocess import STDOUT, check_call, check_output

# get package
packages = check_output(['ls', '/root/pkg']).split()
packages = sorted(packages, reverse=True)
oneclient_package = [path for path in packages
                     if path.startswith('oneclient-base') and
                     not path.startswith('oneclient-base-debuginfo')][0]

# install package
check_call(['sh', '-c', 'dpkg -i /root/pkg/{package} ; apt-get -f -y '
                        'install'.format(package=oneclient_package)
            ], stderr=STDOUT)

# validate package installation
check_call(['/usr/bin/oneclient', '--help'])

sys.exit(0)
