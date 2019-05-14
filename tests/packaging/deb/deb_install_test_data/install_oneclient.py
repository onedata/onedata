import sys
from subprocess import STDOUT, check_call, check_output

# get package
packages = check_output(['ls', '/root/pkg']).split()
packages = sorted(packages, reverse=True)
oneclient_package = [path for path in packages
                     if path.startswith('oneclient') and
                     not path.startswith('oneclient-debuginfo')][0]

onedatafs_py2_package = [path for path in packages
                         if path.startswith('python-onedatafs')][0]

onedatafs_py3_package = [path for path in packages
                         if path.startswith('python3-onedatafs')][0]


# install oneclient package
check_call(['sh', '-c', 'dpkg -i /root/pkg/{package} ; apt-get -f -y '
                        'install'.format(package=oneclient_package)
            ], stderr=STDOUT)

# install onedatafs Python2 package
check_call(['sh', '-c', 'dpkg -i /root/pkg/{package} ; apt-get -f -y '
                        'install'.format(package=onedatafs_py2_package)
            ], stderr=STDOUT)

# install onedatafs Python3 package
check_call(['sh', '-c', 'dpkg -i /root/pkg/{package} ; apt-get -f -y '
                        'install'.format(package=onedatafs_py3_package)
            ], stderr=STDOUT)

# validate oneclient package installation
check_call(['/usr/bin/oneclient', '--help'])

# validate onedatafs Python2 package installation
check_call(['python2', '-c', 'from onedatafs import OnedataFS'])

# validate onedatafs Python3 package installation
check_call(['python3', '-c', 'from onedatafs import OnedataFS'])

sys.exit(0)
