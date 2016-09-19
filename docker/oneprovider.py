#!/usr/bin/env python
# -*- coding: utf-8 -*-

import json
import os
import re
import requests
import shutil
import subprocess as sp
import sys
import time
from requests.packages.urllib3.exceptions import InsecureRequestWarning

requests.packages.urllib3.disable_warnings(InsecureRequestWarning)

ROOT = '/volumes/persistence'
DIRS = ['/etc/op_panel', '/etc/op_worker', '/etc/cluster_manager',
    '/etc/rc.d/init.d', '/var/lib/op_panel', '/var/lib/op_worker',
    '/var/lib/cluster_manager', '/usr/lib64/cluster_manager',
    '/opt/couchbase/var/lib/couchbase', '/var/log/op_panel',
    '/var/log/op_worker', '/var/log/cluster_manager']
ADMIN = os.environ.get('ONEPANEL_ADMIN_USERNAME', 'admin')
PASSWORD = os.environ.get('ONEPANEL_ADMIN_PASSWORD', 'password')

def log(message, end='\n'):
    with open('/proc/1/fd/1', 'w') as f:
        f.write(message + end)
        f.flush()

def replace(file_path, pattern, value):
    with open(file_path, 'rw+') as f:
        content = f.read()
        content = re.sub(pattern, value, content)
        f.seek(0)
        f.truncate()
        f.write(content)

def copy_missing_files():
    for rootdir in DIRS:
        for subdir, _, files in os.walk(rootdir):
            subdir_path = os.path.join(ROOT, subdir[1:])
            if not os.path.exists(subdir_path):
                stat = os.stat(subdir)
                os.makedirs(subdir_path)
                os.chown(subdir_path, stat.st_uid, stat.st_gid)

            for f in files:
                source_path = os.path.join(subdir, f)
                dest_path = os.path.join(subdir_path, f)
                if not os.path.exists(dest_path):
                    stat = os.stat(source_path)
                    shutil.copy(source_path, dest_path)
                    os.chown(dest_path, stat.st_uid, stat.st_gid)

def remove_dirs():
    for rootdir in DIRS:
        if not os.path.islink(rootdir):
            shutil.rmtree(rootdir)

def link_dirs():
    for dest_path in DIRS:
        if not os.path.islink(dest_path):
            source_path = os.path.join(ROOT, dest_path[1:])
            os.symlink(source_path, dest_path)

def set_node_name(file_path):
    hostname = sp.check_output(['hostname', '-f']).rstrip('\n')
    replace(file_path, r'-name .*', '-name onepanel@{0}'.format(hostname))

def set_advertise_address(file_path, advertise_address):
    replace(file_path, r'{advertise_address, .*}',
            '{{advertise_address, "{0}"}}'.format(advertise_address))

def start_service(service_name, stdout=None):
    try:
        sp.check_call(['service', service_name, 'start'])
        log('Starting {0}: [  OK  ]'.format(service_name))
    except Exception:
        pass

def start_services():
    start_service('couchbase-server')
    start_service('cluster_manager')
    start_service('op_worker')

def is_configured():
    try:
        r = requests.get('https://127.0.0.1:9443/api/v3/onepanel/provider/configuration',
                         auth=(ADMIN, PASSWORD),
                         verify=False)
        return r.status_code != 404
    except Exception:
        time.sleep(1)
        return is_configured()

def format_step(step):
    service, action = step.split(':')
    return '* {0}: {1}'.format(service, action)

def configure(config):
    r = requests.post(
        'https://127.0.0.1:9443/api/v3/onepanel/provider/configuration',
        auth=(ADMIN, PASSWORD),
        headers={'content-type': 'application/x-yaml'},
        data=config,
        verify=False)

    if r.status_code != 201:
        log('\nFailed to start configuration process\n{0}'.format(r.text))
        return False

    loc = r.headers['location']
    status = 'running'
    steps = []
    resp = {}

    log('\nConfiguring oneprovider:')
    while status == 'running':
        r = requests.get('https://127.0.0.1:9443' + loc,
                         auth=(ADMIN, PASSWORD),
                         verify=False)
        if r.status_code != 200:
            log('Unexpected configuration error\n{0}'.format(r.text))
            return False
        else:
            resp = json.loads(r.text)
            status = resp.get('status', 'error')
            for step in resp.get('steps', []):
                if steps and step == steps[0]:
                    steps = steps[1:]
                else:
                    log(format_step(step))
            steps = resp.get('steps', [])
            time.sleep(1)

    if status != 'ok':
        log('Error: {0}'.format(resp.get('error', 'unknown')))
        log('Description: {0}'.format(resp.get('description', '-')))
        log('Module: {0}'.format(resp.get('module', '-')))
        log('Function: {0}'.format(resp.get('function', '-')))
        log('Hosts: {0}'.format(', '.join(resp.get('hosts', []))))
        log('For more information please check the logs.')
        return False

    return True

def get_container_id():
    with open('/proc/self/cgroup', 'r') as f:
        return f.readline().split('/')[-1].rstrip('\n')

def inspect_container(container_id):
    try:
        result = sp.check_output(['curl', '-s', '--unix-socket',
                                  '/var/run/docker.sock', 'http:/containers/{0}/json'.
                                 format(container_id)])
        return json.loads(result)
    except Exception:
        return {}

def show_ip_address(json):
    ip = '-'
    try:
        ip = sp.check_output(['hostname', '-i']).rstrip('\n')
        ip = j['NetworkSettings']['Networks'].items()[0][1]['IPAddress']
    except Exception:
        pass
    log('* IP Address: {0}'.format(ip))

def show_ports(json):
    ports = json.get('NetworkSettings', {}).get('Ports', {})
    ports_format = []
    for container_port in ports:
        host = ports[container_port]
        if host:
            for host_port in host:
                ports_format.append('{0}:{1} -> {2}'.format(host_port['HostIp'],
                    host_port['HostPort'], container_port))
        else:
            ports_format.append(container_port)
    ports_str = '\n         '.join(ports_format) if ports_format else '-'
    log('* Ports: {0}'.format(ports_str))

def show_details():
    log('\nContainer details:')

    container_id = get_container_id()
    json = inspect_container(container_id)

    show_ip_address(json)
    show_ports(json)

def infinite_loop():
    while True:
        time.sleep(60)

if __name__ == '__main__':
    copy_missing_files()
    remove_dirs()
    link_dirs()

    set_node_name('/etc/op_panel/vm.args')

    advertise_address = os.environ.get('ONEPANEL_ADVERTISE_ADDRESS')
    if advertise_address:
        set_advertise_address('/etc/op_panel/app.config', advertise_address)

    start_service('op_panel')

    configured = is_configured()

    if configured:
        start_services()
    else:
        batch_mode = os.environ.get('ONEPANEL_BATCH_MODE', 'false')
        batch_config = os.environ.get('ONEPROVIDER_CONFIG', '')
        if batch_mode.lower() == 'true':
            configured = configure(batch_config)

    show_details()

    if configured:
        log('\nCongratulations! oneprovider has been successfully started.')
