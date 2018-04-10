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


LOGS = [('[op_panel]', '/var/log/op_panel'),
        ('[cluster_manager]', '/var/log/cluster_manager'),
        ('[op_worker]', '/var/log/op_worker')]
LOG_LEVELS = ['debug', 'info', 'error']
ONEPANEL_OVERRIDE = 'ONEPANEL_OVERRIDE'

APP_CONFIG_SOURCES_PATH = '_build/default/rel/op_panel/etc/app.config'
VM_ARGS_SOURCES_PATH = '_build/default/rel/op_panel/etc/vm.args'

APP_CONFIG_PACKAGES_PATH = '/etc/op_panel/app.config'
VM_ARGS_PACKAGES_PATH = '/etc/op_panel/vm.args'


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
        if os.environ.get(ONEPANEL_OVERRIDE):
            sp.check_call([os.path.join(os.environ.get(ONEPANEL_OVERRIDE),
                           "_build/default/rel/op_panel/bin/op_panel"),
                           'start'])
        else:
            sp.check_call(['service', 'op_panel', 'start'], stdout=null,
                          stderr=null)

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

def is_configured(config):
    users = get_users(config) + [('admin', 'password')]
    r = do_request(users, requests.get,
                   'https://127.0.0.1:9443/api/v3/onepanel/provider/configuration',
                   verify=False)
    if r.status_code == 200:
        return True
    elif r.status_code == 404:
        return False
    else:
        raise ValueError('Error checking deployment status')


def format_error(action, response):
    return 'Error {action}: {error}\nDescription: {description}\n'
    'Module: {module}\nFunction: {function}\nHosts: {hosts}\n'
    'For more information please check the logs.'.format(
        action=action,
        error=response.get('error', 'unknown'),
        description=response.get('description', '-'),
        module=response.get('module', '-'),
        function=response.get('function', '-'),
        hosts=', '.join(response.get('hosts', [])))


def restart_oneprovider(config):
    users = get_users(config) + [('admin', 'password')]
    print('Stopping services to be sure')
    stop_request = do_request(users, requests.patch,
                              'https://127.0.0.1:9443/api/v3/onepanel/provider/services?started=false',
                              headers={'content-type': 'application/json'},
                              verify=False)
    if stop_request.status_code != 204:
        errdata = json.loads(stop_request.text)
        raise ValueError(format_error('stopping oneprovider', errdata))
    print('Starting services')
    start_request = do_request(users, requests.patch,
                               'https://127.0.0.1:9443/api/v3/onepanel/provider/services?started=true',
                               headers={'content-type': 'application/json'},
                               verify=False)
    if start_request.status_code != 204:
        errdata = json.loads(stop_request.text)
        raise ValueError(format_error('starting oneprovider', errdata))


if __name__ == '__main__':
    try:
        sp.call(['/root/persistence-dir.py', '--copy-missing-files'])

        if os.environ.get(ONEPANEL_OVERRIDE):
            app_config_path = os.path.join(os.environ.get(ONEPANEL_OVERRIDE),
                                           APP_CONFIG_SOURCES_PATH)
            vm_args_path = os.path.join(os.environ.get(ONEPANEL_OVERRIDE),
                                        VM_ARGS_SOURCES_PATH)
        else:
            app_config_path = APP_CONFIG_PACKAGES_PATH
            vm_args_path = VM_ARGS_PACKAGES_PATH

        set_node_name(vm_args_path)

        advertise_address = os.environ.get('ONEPANEL_ADVERTISE_ADDRESS')
        if advertise_address:
            set_advertise_address(app_config_path, advertise_address)

        generate_test_web_cert = os.environ.get('ONEPANEL_GENERATE_TEST_WEB_CERT')
        if generate_test_web_cert:
            domain = os.environ.get('ONEPANEL_GENERATED_CERT_DOMAIN')
            set_generate_test_web_cert(app_config_path, generate_test_web_cert,
                                       domain)

        trust_test_ca = os.environ.get('ONEPANEL_TRUST_TEST_CA')
        if trust_test_ca:
            set_trust_test_ca(app_config_path, trust_test_ca)

        batch_config = get_batch_config()
        onezone_domain = get_onezone_domain(batch_config)
        set_onezone_domain(onezone_domain)

        start_onepanel()

        configured = is_configured(batch_config)
        batch_mode = os.environ.get('ONEPANEL_BATCH_MODE', 'false')

        print('configured: {}'.format(configured))

        if configured:
            print('Starting existing oneprovider')
            restart_oneprovider(batch_config)
            configured = True
        elif batch_mode.lower() == 'true':
            print('Configuring oneprovider')
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
