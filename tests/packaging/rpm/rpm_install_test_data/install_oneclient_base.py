import sys
from subprocess import STDOUT, check_call, check_output

release = sys.argv[1]

# get packages
packages = check_output(['ls', '/root/pkg']).split()
packages = sorted(packages, reverse=True)
oneclient_package = [path for path in packages
                     if path.startswith('onedata{release}-oneclient-base'.format(release=release)) and
                     not path.startswith('onedata{release}-oneclient-base-debuginfo'.format(release=release))][0]

onedatafs_py2_package = [path for path in packages
                         if path.startswith('onedata{release}-python2-onedatafs'.format(release=release))][0]

onedatafs_py3_package = [path for path in packages
                         if path.startswith('onedata{release}-python3-onedatafs'.format(release=release))][0]

# install oneclient package
check_call(['yum', '-y', '--enablerepo=onedata', 'install',
            '/root/pkg/' + oneclient_package], stderr=STDOUT)

# install onedatafs packages for Python2
check_call(['yum', '-y', '--enablerepo=onedata', 'install',
            '/root/pkg/' + onedatafs_py2_package], stderr=STDOUT)

# install onedatafs packages for Python3
check_call(['yum', '-y', '--enablerepo=onedata', 'install',
            '/root/pkg/' + onedatafs_py3_package], stderr=STDOUT)


# validate oneclient package installation
check_call(['scl', 'enable', 'onedata{release}'.format(release=release), 'oneclient --help'])

# validate onedatafs Python2 package installation
check_call(['scl', 'enable', 'onedata{release}'.format(release=release), 'PYTHONPATH="${ONEDATA_PYTHON2_PATH}" python2 -c "from onedatafs import OnedataFS"'])

# validate onedatafs Python3 package installation
check_call(['scl', 'enable', 'onedata{release}'.format(release=release), 'PYTHONPATH="${ONEDATA_PYTHON3_PATH}" python3 -c "from onedatafs import OnedataFS"'])

sys.exit(0)
