import sys
from subprocess import STDOUT, check_call, check_output

dist = sys.argv[1]

# get package
packages = check_output(['ls', '/root/pkg']).split()
packages = sorted(packages, reverse=True)
oneclient_package = [path for path in packages
                     if path.startswith('oneclient')
                     and (dist in path)
                     and not ((path.startswith('oneclient-base')
                          or path.startswith('oneclient-debuginfo')))][0]

# install package
check_call(['sh', '-c', 'dpkg -i /root/pkg/{package} ; apt-get -f -y '
                        'install'.format(package=oneclient_package)
            ], stderr=STDOUT)

# validate package installation
check_call(['/usr/bin/oneclient', '--help'])

sys.exit(0)
