import sys
from subprocess import STDOUT, check_call, check_output

dist = sys.argv[1]

# get package
packages = check_output(['ls', '/root/pkg']).split()
packages = sorted(packages, reverse=True)
oneclient_package = [path for path in packages
                     if path.startswith('oneclient-base')
                     and (dist in path)
                     and not path.startswith('oneclient-base-debuginfo')][0]

onedatafs_py2_package = [path for path in packages
                         if path.startswith('python-onedatafs')
                         and (dist in path)][0]

onedatafs_py3_package = [path for path in packages
                         if path.startswith('python3-onedatafs')
                         and (dist in path)][0]


# install oneclient package
check_call(['sh', '-c', 'apt -y install /root/pkg/{package}'.format(package=oneclient_package)
            ], stderr=STDOUT)

# install onedatafs Python2 package
check_call(['sh', '-c', 'apt -y install /root/pkg/{package}'.format(package=onedatafs_py2_package)
            ], stderr=STDOUT)

# install onedatafs Python3 package
check_call(['sh', '-c', 'apt -y install /root/pkg/{package}'.format(package=onedatafs_py3_package)
            ], stderr=STDOUT)

# validate oneclient package installation
check_call(['/usr/bin/oneclient', '--help'])

# validate onedatafs Python2 package installation
check_call(['python2', '-c', 'from onedatafs import OnedataFS'])

# validate onedatafs Python3 package installation
check_call(['python3', '-c', 'from onedatafs import OnedataFS'])

sys.exit(0)
