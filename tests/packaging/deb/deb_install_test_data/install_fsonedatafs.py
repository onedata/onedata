import sys
from subprocess import STDOUT, check_call, check_output

dist = sys.argv[1]

# get package
packages = check_output(['ls', '/root/pkg']).split()
packages = sorted(packages, reverse=True)

onedatafs_py2_package = [path for path in packages
                         if path.startswith('python-onedatafs')
                         and (dist in path)][0]

onedatafs_py3_package = [path for path in packages
                         if path.startswith('python3-onedatafs')
                         and (dist in path)][0]

fsonedatafs_py2_package = [path for path in packages
                         if path.startswith('python-fs-plugin-onedatafs')
                         and (dist in path)][0]

fsonedatafs_py3_package = [path for path in packages
                         if path.startswith('python3-fs-plugin-onedatafs')
                         and (dist in path)][0]

# install Python prerequisites
check_call(['apt', '-y', 'install', 'python-pip', 'python3-pip'])
check_call(['pip', 'install', 'setuptools', 'six', 'fs'])
check_call(['pip3', 'install', 'setuptools', 'six', 'fs'])

# install onedatafs Python2 package
check_call(['sh', '-c', 'apt -y install /root/pkg/{package}'.format(package=onedatafs_py2_package)
            ], stderr=STDOUT)

# install onedatafs Python3 package
check_call(['sh', '-c', 'apt -y install /root/pkg/{package}'.format(package=onedatafs_py3_package)
            ], stderr=STDOUT)

# install fs-onedatafs Python2 package
check_call(['sh', '-c', 'apt -y install /root/pkg/{package}'.format(package=fsonedatafs_py2_package)
            ], stderr=STDOUT)

# install fs-onedatafs Python3 package
check_call(['sh', '-c', 'apt -y install /root/pkg/{package}'.format(package=fsonedatafs_py3_package)
            ], stderr=STDOUT)

# validate fs-onedatafs Python2 package installation
check_call(['python2', '-c', 'from fs.onedatafs import OnedataFS'])

# validate fs-onedatafs Python3 package installation
check_call(['python3', '-c', 'from fs.onedatafs import OnedataFS'])

sys.exit(0)
