import sys
from subprocess import STDOUT, check_call, check_output

# get packages
packages = check_output(['ls', '/root/pkg']).split()
packages = sorted(packages, reverse=True)
onedatafs_py2_package = [path for path in packages
                         if path.startswith('onedata{release}-python2-onedatafs'.format(release='2002'))][0]

onedatafs_py3_package = [path for path in packages
                         if path.startswith('onedata{release}-python3-onedatafs'.format(release='2002'))][0]

fsonedatafs_py2_package = [path for path in packages
                         if path.startswith('onedata{release}-python2-fs-onedatafs'.format(release='2002'))][0]

fsonedatafs_py3_package = [path for path in packages
                         if path.startswith('onedata{release}-python3-fs-onedatafs'.format(release='2002'))][0]

# install Python prerequisites
check_call(['yum', '-y', 'install', 'python2-pip', 'python36-pip'])
check_call(['pip', 'install', 'setuptools', 'six', 'fs'])
check_call(['pip3', 'install', 'setuptools', 'six', 'fs'])

# install onedatafs packages for Python2
check_call(['yum', '-y', '--enablerepo=onedata', 'install',
            '/root/pkg/' + onedatafs_py2_package], stderr=STDOUT)

# install onedatafs packages for Python3
check_call(['yum', '-y', '--enablerepo=onedata', 'install',
            '/root/pkg/' + onedatafs_py3_package], stderr=STDOUT)

# install fsonedatafs packages for Python2
check_call(['yum', '-y', '--enablerepo=onedata', 'install',
            '/root/pkg/' + fsonedatafs_py2_package], stderr=STDOUT)

# install fsonedatafs packages for Python3
check_call(['yum', '-y', '--enablerepo=onedata', 'install',
            '/root/pkg/' + fsonedatafs_py3_package], stderr=STDOUT)

# validate onedatafs Python2 package installation
check_call(['scl', 'enable', 'onedata{release}'.format(release='2002'), 'PYTHONPATH="${ONEDATA_PYTHON2_PATH}" python2 -c "from fs.onedatafs import OnedataFS"'])

# validate onedatafs Python3 package installation
check_call(['scl', 'enable', 'onedata{release}'.format(release='2002'), 'PYTHONPATH="${ONEDATA_PYTHON3_PATH}" python3 -c "from fs.onedatafs import OnedataFS"'])

sys.exit(0)
