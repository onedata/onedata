"""This module contains net utility functions for env_up, acceptance
and performance tests.
"""
__author__ = "Lukasz Opiola, Jakub Kudzia"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

from tests import *
from tests.utils.utils import run_os_command

import dns.resolver
import re
import requests
import time

requests.packages.urllib3.disable_warnings()


def dns_lookup(host, dns_addr):
    """Helper function to check host's ip using given dns"""
    resolver = dns.resolver.Resolver()
    resolver.nameservers = [dns_addr]
    answer = resolver.query(host)
    answer = answer.response.answer[0].to_text()
    # Find an IP address in response and return it
    # dns resolver doesn't seem to have a straightforward way to get IP by hostname
    pattern = re.compile('\\b\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\b')
    pattern.findall(answer)
    return pattern.findall(answer)[0]


def ping(ip):
    """Helper function that checks if given ip is reachable with ping"""
    return 0 == run_os_command(['ping', '-c 1', ip], output=False)


def http_get(ip, port, path, use_ssl=True, headers = None, verify=False,
             cert=None, auth=None):
    """Helper function that perform a HTTP GET request
    Returns a tuple (Code, Headers, Body)
    """
    protocol = 'https' if use_ssl else 'http'
    response = requests.get('{0}://{1}:{2}{3}'.format(protocol, ip, port, path),
                            verify=verify, headers=headers, timeout=10,
                            cert=cert, auth=auth)
    return response.status_code, response.headers, response.text


def http_post(ip, port, path, use_ssl=True, data=None, headers=None,
              verify=False, cert=None, auth=None):
    """Helper function that perform a HTTP POST request
    Returns a tuple (Code, Headers, Body)
    """
    protocol = 'https' if use_ssl else 'http'
    response = requests.post('{0}://{1}:{2}{3}'.format(protocol, ip, port, path),
                             data, verify=verify, headers=headers, timeout=10,
                             cert=cert, auth=auth)
    return response.status_code, response.headers, response.text


def http_delete(ip, port, path, use_ssl=True, headers=None, verify=False,
                cert=None, auth=None):
    """Helper function that perform a HTTP DELETE request
    Returns a tuple (Code, Headers, Body)
    """
    protocol = 'https' if use_ssl else 'http'
    response = requests.delete('{0}://{1}:{2}{3}'.format(protocol, ip, port, path),
                               verify=verify, headers=headers, timeout=10,
                               cert=cert, auth=auth)
    return response.status_code, response.headers, response.text


def http_put(ip, port, path, use_ssl=True, data=None, headers=None,
             verify=False, cert=None, auth=None):
    """Helper function that perform a HTTP PUT request
    Returns a tuple (Code, Headers, Body)
    """
    protocol = 'https' if use_ssl else 'http'
    response = requests.put('{0}://{1}:{2}{3}'.format(protocol, ip, port, path),
                            verify=verify, headers=headers, timeout=10,
                            cert=cert, data=data, auth=auth)
    return response.status_code, response.headers, response.text


def check_http_connectivity(ip, port, path, expected_code, use_ssl=True, number_of_retries=20):
    """Helper function to check https connectivity
    Retries once a second, up to given number of times.
    """
    if number_of_retries == 0:
        protocol = 'https' if use_ssl else 'http'
        raise Exception('{0}://{1}:{2}{3} is unreachable.'.format(protocol, ip, port, path))
    else:
        try:
            status_code, _, _ = http_get(ip, port, path, use_ssl)
            return expected_code == status_code
        except:
            time.sleep(1)
            return check_http_connectivity(ip, port, path, expected_code, use_ssl, number_of_retries - 1)


def oz_rest_path(*args):
    return rest_path(OZ_REST_PATH_PREFIX, *args)


def panel_rest_path(*args):
    return rest_path(PANEL_REST_PATH_PREFIX, *args)


def rest_path(prefix, *args):
    full_path = [prefix]
    full_path.extend(args)
    return "/".join(full_path)

