import json
import requests
import sys
import time
from requests.packages.urllib3.exceptions import InsecureRequestWarning
from subprocess import STDOUT, check_call, check_output

requests.packages.urllib3.disable_warnings(InsecureRequestWarning)

EMERGENCY_USERNAME = 'onepanel'
EMERGENCY_PASSPHRASE = 'passphrase'
release = sys.argv[1]

# get packages
packages = check_output(['ls', '/root/pkg']).split()
packages = sorted(packages, reverse=True)
op_panel_package = \
    [path for path in packages
            if path.startswith('onedata{release}-op-panel'.format(release=release)) and path.endswith('.rpm')][0]
cluster_manager_package = \
    [path for path in packages
            if path.startswith('onedata{release}-cluster-manager'.format(release=release)) and path.endswith('.rpm')][0]
op_worker_package = \
    [path for path in packages
            if path.startswith('onedata{release}-op-worker'.format(release=release)) and path.endswith('.rpm')][0]
oneprovider_package = \
    [path for path in packages
            if path.startswith('onedata{release}-oneprovider'.format(release=release)) and path.endswith('.rpm')][0]

# get couchbase
check_call(['wget', 'http://packages.onedata.org/yum/centos/7x/x86_64/'
                    'couchbase-server-community-4.5.1-centos7.x86_64.rpm'])

# Inject Overlay config to accept test CA certificate
check_call(['mkdir', '-p', '/opt/onedata/onedata{release}/root/etc/op_panel'.format(release=release)])
check_call(['cp', '/root/data/overlay.config',
                  '/opt/onedata/onedata{release}/root/etc/op_panel/overlay.config'.format(release=release)])

# install packages
check_call(['yum', '-y', 'install',
            './couchbase-server-community-4.5.1-centos7.x86_64.rpm'],
           stderr=STDOUT)
check_call(['yum', '-y', '--enablerepo=onedata', 'install',
            '/root/pkg/' + op_panel_package], stderr=STDOUT)
check_call(['yum', '-y', 'install', '--enablerepo=onedata',
            '/root/pkg/' + cluster_manager_package], stderr=STDOUT)
check_call(['yum', '-y', 'install', '--enablerepo=onedata',
            '/root/pkg/' + op_worker_package], stderr=STDOUT)
check_call(['yum', '-y', 'install', '--enablerepo=onedata',
            '/root/pkg/' + oneprovider_package], stderr=STDOUT)

# validate packages installation
check_call(['service', 'op_panel'.format(release=release), 'status'])

# configure oneprovider
with open('/root/data/config.yml', 'r') as f:
    r = requests.put('https://127.0.0.1:9443/api/v3/onepanel/emergency_passphrase',
                     headers={'content-type': 'application/json'},
                     data=json.dumps({'newPassphrase': EMERGENCY_PASSPHRASE}),
                     verify=False)
    assert r.status_code == 204

    r = requests.post(
        'https://127.0.0.1:9443/api/v3/onepanel/provider/configuration',
        auth=(EMERGENCY_USERNAME, EMERGENCY_PASSPHRASE),
        headers={'content-type': 'application/x-yaml'},
        data=f.read(),
        verify=False)

    loc = r.headers['location']
    status = 'running'
    while status == 'running':
        r = requests.get('https://127.0.0.1:9443' + loc,
                         auth=(EMERGENCY_USERNAME, EMERGENCY_PASSPHRASE),
                         verify=False)
        print(r.text)
        assert r.status_code == 200
        status = json.loads(r.text)['status']
        time.sleep(5)

assert status == 'ok'

# validate oneprovider configuration
check_call(['service', 'cluster_manager'.format(release=release), 'status'])
check_call(['service', 'op_worker'.format(release=release), 'status'])

# stop oneprovider services
for service in ['workers', 'managers', 'databases']:
    r = requests.patch(
        'https://127.0.0.1:9443/api/v3/onepanel/provider/{0}?started=false'.format(
            service),
        auth=(EMERGENCY_USERNAME, EMERGENCY_PASSPHRASE),
        headers={'content-type': 'application/json'},
        verify=False)
    assert r.status_code == 204

sys.exit(0)
