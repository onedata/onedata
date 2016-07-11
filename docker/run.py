#!/usr/bin/env python
# -*- coding: utf-8 -*-

import json
import os
import time
import re
import shutil
import socket
import subprocess as sp
import sys

ROOT = '/volumes/persistency'
DIRS = ['/etc/op_panel', '/etc/op_worker', '/etc/cluster_manager',
    '/etc/rc.d/init.d', '/var/lib/op_panel', '/var/lib/op_worker',
    '/var/lib/cluster_manager', '/usr/lib64/op_panel',
    '/opt/couchbase/var/lib/couchbase', '/var/log/op_panel',
    '/var/log/op_worker', '/var/log/cluster_manager']

def log(message, end='\n'):
    sys.stdout.write(message + end)
    sys.stdout.flush()

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
    hostname = socket.getfqdn()
    replace(file_path, r'-name .*', '-name onepanel@{0}'.format(hostname))

def set_multicast_address(file_path, multicast_address):
    replace(file_path, r'{multicast_address, .*}',
        '{{multicast_address, "{0}"}}'.format(multicast_address))

def start_service(service_name, stdout=None):
    with open(os.devnull, 'w') as stderr:
        sp.check_call(['service', service_name, 'start'], stdout=stdout,
            stderr=stderr)

def start_services():
    log('Starting couchbase-server: ', '')
    with open(os.devnull, 'w') as stdout:
        start_service('couchbase-server', stdout)
    log('[  OK  ]')
    start_service('cluster_manager')
    time.sleep(5)
    start_service('op_worker')
    log('\nCongratulations! oneprovider has been successfully started.')

def is_configured():
    return 'undefined' not in sp.check_output(['op_panel_admin', '--config'])

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
        time.sleep(1)

if __name__ == '__main__':
    copy_missing_files()
    remove_dirs()
    link_dirs()

    set_node_name('/etc/op_panel/vm.args')

    multicast_address = os.environ.get('ONEPANEL_MULTICAST_ADDRESS')
    if multicast_address:
        set_multicast_address('/etc/op_panel/app.config', multicast_address)

    start_service('op_panel')

    if is_configured():
        start_services()
    else:
        batch_mode = os.environ.get('ONEPANEL_BATCH_MODE')
        batch_cofig = os.environ.get('ONEPANEL_BATCH_MODE_CONFIG', '')
        if batch_mode and batch_mode.lower() == 'true':
            sp.check_call(['op_panel_admin', '--install', batch_cofig])

    show_details()
    infinite_loop()
