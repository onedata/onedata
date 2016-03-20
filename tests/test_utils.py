import inspect
import py.process
import dns.resolver
import re
import requests
import time


def test_file(relative_file_path):
    """Returns a path to file located in {test_name}_data directory, where
    {test_name} is name of the test module that called this function.
    example: using tesutils.test_file('my_file') in my_test.py will return 'tests/my_test_data/my_file'
    """
    caller = inspect.stack()[1]
    caller_mod = inspect.getmodule(caller[0])
    caller_mod_file_path = caller_mod.__file__
    return '{0}_data/{1}'.format(caller_mod_file_path.rstrip('.py'), relative_file_path)


def run_command(string_or_list):
    """Runs a given command and returns unicode output.
    The argument may be:
    1) a full command: 'ls -al'
    2) list of strings to be joined with spaces: ['ls' '-al']
    """
    if isinstance(string_or_list, list):
        string_or_list = ' '.join(string_or_list)
    elif not isinstance(string_or_list, str):
        raise ValueError("argument must be a string or a list of strings")
    # Execute the command and remove trailing whitespaces
    return py.process.cmdexec(string_or_list).rstrip()


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
    return '0' == run_command(['ping', '-c 1', ip, '1>/dev/null;', 'echo $?'])


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
