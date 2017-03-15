"""Utils for managing user account information/environment
"""


__author__ = "Jakub Liput, Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class User(object):
    def __init__(self, name, password):
        self.name = name
        self.password = password
