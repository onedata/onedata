import dns.resolver
import re
import requests
import time

from tests.utils.utils import run_os_command


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
    print "PING: " ,run_os_command(['ping', '-c 1', ip], output=False)
    return 0 == run_os_command(['ping', '-c 1', ip], output=False)


def http_get(ip, port, path, use_ssl):
    """Helper function that perform a HTTP GET request
    Returns a tuple (Code, Headers, Body)
    """
    protocol = 'https' if use_ssl else 'http'
    response = requests.get('{0}://{1}:{2}{3}'.format(protocol, ip, port, path), verify=False, timeout=10)
    return response.status_code, response.headers, response.text


def http_post(ip, port, path, use_ssl, data):
    """Helper function that perform a HTTP GET request
    Returns a tuple (Code, Headers, Body)
    """
    protocol = 'https' if use_ssl else 'http'
    response = requests.post('{0}://{1}:{2}{3}'.format(protocol, ip, port, path),
                             data, verify=False, timeout=10)
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
