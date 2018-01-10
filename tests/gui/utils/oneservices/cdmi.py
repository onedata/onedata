"""Utils for managing REST API for CDMI service
"""

from base64 import b64encode

import requests

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class CDMIClient(object):
    def __init__(self, provider_ip, auth_token, cdmi_version='1.1.1', port=443):
        self.provider_ip = provider_ip
        self.auth_token = auth_token
        self.cdmi_version = cdmi_version
        self.port = port

    def create_file(self, path, text=''):
        url = 'https://{ip}:{port}/cdmi{path}'.format(ip=self.provider_ip,
                                                      port=self.port,
                                                      path=path)
        headers = {'X-CDMI-Specification-Version': self.cdmi_version,
                   'X-Auth-Token': self.auth_token,
                   'content-type': 'application/cdmi-object'}
        return requests.put(url, headers=headers, verify=False,
                            json={"value": text})

    def write_to_file(self, path, text, offset=0):
        start = offset
        end = start + len(text) - 1
        text = b64encode(text)
        url_template = 'https://{ip}:{port}/cdmi{path}?value:{start}-{end}'
        url = url_template.format(ip=self.provider_ip, port=self.port,
                                  path=path, start=start, end=end)
        headers = {'X-CDMI-Specification-Version': self.cdmi_version,
                   'X-Auth-Token': self.auth_token,
                   'content-type': 'application/cdmi-object',
                   'X-CDMI-Partial': 'true'}
        return requests.put(url, headers=headers, verify=False,
                            json={"value": text})

    def read_from_file(self, path, read_range=None):
        url = 'https://{ip}:{port}/cdmi{path}'.format(ip=self.provider_ip,
                                                      port=self.port,
                                                      path=path)
        headers = {'X-Auth-Token': self.auth_token}
        if read_range:
            headers['Range'] = 'bytes={start}-{end}'.format(start=read_range[0],
                                                            end=read_range[1])
        return requests.get(url, headers=headers, verify=False).content
