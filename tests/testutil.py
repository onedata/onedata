import inspect
import py.process
import requests
import time
import dns.resolver
import re


# Returns a path to file located in {test_name}_data directory, where
# {test_name} is name of the test module that called this function.
# example: using tesutils.test_file('my_file') in my_test.py will return 'tests/my_test_data/my_file'
def test_file(relative_file_path):
    caller = inspect.stack()[1]
    caller_mod = inspect.getmodule(caller[0])
    caller_mod_file_path = caller_mod.__file__
    return '{0}_data/{1}'.format(caller_mod_file_path.rstrip('.py'), relative_file_path)


# Runs a given command and returns unicode output.
# The argument may be:
# 1) a full command: 'ls -al'
# 2) list of strings to be joined with spaces: ['ls' '-al']
def run_command(string_or_list):
    if isinstance(string_or_list, list):
        string_or_list = ' '.join(string_or_list)
    elif not isinstance(string_or_list, str):
        raise ValueError("argument must be a string or a list of strings")
    # Execute the command and remove trailing whitespaces
    return py.process.cmdexec(string_or_list).rstrip()


# Helper function to check host's ip using given dns
def dns_lookup(dns_addr, host):
    resolver = dns.resolver.Resolver()
    resolver.nameservers = [dns_addr]
    answer = resolver.query(host)
    answer = answer.response.answer[0].to_text()
    # Find an IP address in response and return it
    # dns resolver doesn't seem to have a straightforward way to get IP by hostname
    pattern = re.compile('\\b\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\b')
    pattern.findall(answer)
    return pattern.findall(answer)[0]


# Helper function that checks if given ip is reachable with ping
def ping(ip):
    return '0' == run_command(['ping', '-c 1', ip, '1>/dev/null;', 'echo $?'])


# Helper function to check https connectivity
# Returned value means if expected status code was the same as actual.
def http_get(ip, port, path, expected_code, use_ssl):
    protocol = 'https' if use_ssl else 'http'
    response = requests.get('{0}://{1}:{2}{3}'.format(protocol, ip, port, path), verify=False, timeout=0.3)
    return expected_code == response.status_code


# Helper function to check https connectivity
# Retries once a second, up to given number of times.
def http_get_retry(ip, port, path, expected_code, use_ssl=True, number_of_retries=20):
    if number_of_retries == 0:
        protocol = 'https' if use_ssl else 'http'
        raise Exception('{0}://{1}:{2}{3} is unreachable.'.format(protocol, ip, port, path))
    else:
        try:
            return http_get(ip, port, path, expected_code, use_ssl)
        except:
            time.sleep(1)
            return http_get_retry(ip, port, path, expected_code, use_ssl, number_of_retries - 1)