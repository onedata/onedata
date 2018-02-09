#!/usr/bin/env python
# -*- coding: utf-8 -*-

import json
import os
import re
import shutil
import subprocess as sp
import sys
import time

import requests
import yaml
from requests.packages.urllib3.exceptions import InsecureRequestWarning

requests.packages.urllib3.disable_warnings(InsecureRequestWarning)

ROOT = '/volumes/persistence'
DIRS = ['/etc/init.d', '/etc/op_worker/certs', '/var/lib/op_panel/mnesia',
        '/opt/couchbase/var/lib/couchbase', '/var/log/op_panel',
        '/var/log/op_worker', '/var/log/cluster_manager']
LOGS = [('[op_panel]', '/var/log/op_panel'),
        ('[cluster_manager]', '/var/log/cluster_manager'),
        ('[op_worker]', '/var/log/op_worker')]
LOG_LEVELS = ['debug', 'info', 'error']


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
        if not os.path.exists(rootdir):
            os.makedirs(rootdir)

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


def set_generate_test_web_cert(file_path, generate_test_web_cert, domain):
    flag = 'true' if generate_test_web_cert == 'true' else 'false'
    replace(file_path, r'{generate_test_web_cert, .*}',
            '{{generate_test_web_cert, {0}}}'.format(flag))
    replace(file_path, r'{test_web_cert_domain, .*}',
            '{{test_web_cert_domain, "{0}"}}'.format(domain))


def set_trust_test_ca(file_path, trust_test_ca):
    flag = 'true' if trust_test_ca == 'true' else 'false'
    replace(file_path, r'{treat_test_ca_as_trusted, .*}',
            '{{treat_test_ca_as_trusted, {0}}}'.format(flag))


def start_onepanel():
    log('Starting op_panel', '\t')
    with open(os.devnull, 'w') as null:
        sp.check_call(['service', 'op_panel', 'start'],
                      stdout=null, stderr=null)
    log('[  OK  ]')


def format_step(step):
    service, action = step.split(':')
    return '* {0}: {1}'.format(service, action)


def get_users(config):
    users_config = config.get('onepanel', {}).get('users', {})
    users = [('admin', 'password')]

    for username, props in users_config.items():
        if props.get('userRole', '') == 'admin':
            users.append((username, props.get('password', '')))

    return users


def get_onezone_domain(config):
    return config.get('onezone', {}).get('domainName', 'onedata.org')


def set_onezone_domain(domain):
    replace('/etc/op_panel/app.config', r'{onezone_domain, .*}',
            '{{onezone_domain, "{0}"}}'.format(domain))
    replace('/etc/op_worker/app.config', r'{oz_domain, .*}',
            '{{oz_domain, "{0}"}}'.format(domain))


def do_request(users, request, *args, **kwargs):
    for (username, password) in users:
        r = request(*args, auth=(username, password), **kwargs)
        if r.status_code != 403:
            return r

    raise ValueError('Authorization error.\n'
                     'Please ensure that valid admin credentials are present\n'
                     'in the onepanel.users section of the configuration.')


def get_batch_config():
    batch_config = os.environ.get('ONEPROVIDER_CONFIG', '')
    batch_config = yaml.load(batch_config)
    if not batch_config:
        return {}
    return batch_config


def configure(config):
    users = get_users(config)
    r = do_request(users, requests.post,
                   'https://127.0.0.1:9443/api/v3/onepanel/provider/configuration',
                   headers={'content-type': 'application/x-yaml'},
                   data=yaml.dump(config),
                   verify=False)

    if r.status_code != 201 and r.status_code != 204:
        raise ValueError(
            'Failed to start configuration process, the response was:\n'
            '  code: {0}\n'
            '  body: {1}\n'
            'For more information please check the logs.'.format(r.status_code,
                                                                 r.text))

    loc = r.headers['location']
    status = 'running'
    steps = []
    resp = {}

    log('\nConfiguring oneprovider:')
    while status == 'running':
        r = do_request(users, requests.get,
                       'https://127.0.0.1:9443' + loc,
                       verify=False)
        if r.status_code != 200:
            raise ValueError('Unexpected configuration error\n{0}'
                             'For more information please check the logs.'.format(
                r.text))
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
        raise ValueError('Error: {error}\nDescription: {description}\n'
                         'Module: {module}\nFunction: {function}\nHosts: {hosts}\n'
                         'For more information please check the logs.'.format(
            error=resp.get('error', 'unknown'),
            description=resp.get('description', '-'),
            module=resp.get('module', '-'),
            function=resp.get('function', '-'),
            hosts=', '.join(resp.get('hosts', []))))


def get_container_id():
    with open('/proc/self/cgroup', 'r') as f:
        return f.readline().split('/')[-1].rstrip('\n')


def inspect_container(container_id):
    try:
        result = sp.check_output(['curl', '-s', '--unix-socket',
                                  '/var/run/docker.sock',
                                  'http:/containers/{0}/json'.
                                 format(container_id)])
        return json.loads(result)
    except Exception:
        return {}


def show_ip_address(json):
    ip = '-'
    try:
        ip = sp.check_output(['hostname', '-i']).rstrip('\n')
        ip = json['NetworkSettings']['Networks'].items()[0][1]['IPAddress']
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
                                                            host_port[
                                                                'HostPort'],
                                                            container_port))
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


def infinite_loop(log_level):
    logs = []
    if log_level in LOG_LEVELS:
        log('\nLogging on \'{0}\' level:'.format(log_level))
        for log_prefix, log_dir in LOGS:
            log_file = os.path.join(log_dir, log_level + '.log')
            logs.append((log_prefix, log_file, None, None))

    while True:
        logs = print_logs(logs)
        time.sleep(1)


def print_logs(logs):
    new_logs = []

    for log_prefix, log_file, log_fd, log_ino in logs:
        try:
            if os.stat(log_file).st_ino != log_ino:
                if log_fd:
                    log_fd.close()
                log_fd = open(log_file, 'r')
                log_ino = os.stat(log_file).st_ino

            log_line = log_fd.readline()
            while log_line:
                log('{0} {1}'.format(log_prefix, log_line), end='')
                log_line = log_fd.readline()

            new_logs.append((log_prefix, log_file, log_fd, log_ino))
        except:
            new_logs.append((log_prefix, log_file, None, None))

    return new_logs


if __name__ == '__main__':
    try:
        copy_missing_files()
        remove_dirs()
        link_dirs()

        set_node_name('/etc/op_panel/vm.args')

        advertise_address = os.environ.get('ONEPANEL_ADVERTISE_ADDRESS')
        if advertise_address:
            set_advertise_address(
                '/etc/op_panel/app.config', advertise_address)

        generate_test_web_cert = os.environ.get(
            'ONEPANEL_GENERATE_TEST_WEB_CERT'
        )
        if generate_test_web_cert:
            domain = os.environ.get('ONEPANEL_GENERATED_CERT_DOMAIN')
            set_generate_test_web_cert(
                '/etc/op_panel/app.config', generate_test_web_cert, domain)

        trust_test_ca = os.environ.get(
            'ONEPANEL_TRUST_TEST_CA'
        )
        if trust_test_ca:
            set_trust_test_ca('/etc/op_panel/app.config', trust_test_ca)

        batch_config = get_batch_config()
        onezone_domain = get_onezone_domain(batch_config)
        set_onezone_domain(onezone_domain)

        start_onepanel()

        configured = False
        batch_mode = os.environ.get('ONEPANEL_BATCH_MODE', 'false')
        if batch_mode.lower() == 'true':
            configure(batch_config)
            configured = True

        show_details()

        if configured:
            log('\nCongratulations! oneprovider has been successfully started.')
    except Exception as e:
        log('\n{0}'.format(e))
        if os.environ.get('ONEPANEL_DEBUG_MODE'):
            pass
        else:
            sys.exit(1)

    log_level = os.environ.get('ONEPANEL_LOG_LEVEL', 'info').lower()

    infinite_loop(log_level)
