"""Common code for installation test script of oneprovider RPM and DEB.
"""
__author__ = "Lukasz Opiola"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import tests.utils.path_utils
from shutil import copyfile
from shutil import move


###################### FUNCTIONS ######################

def update_token_in_config(config_file, token):
    backup_config_file = config_file + '.bak'
    copyfile(config_file, backup_config_file)
    with open(config_file, 'r') as f:
        contents = f.read()
    contents = contents.replace('{{registration_token}}', token)
    with open(config_file, 'w') as f:
        f.write(contents)


def reset_token_in_config(config_file):
    backup_config_file = config_file + '.bak'
    move(backup_config_file, config_file)
