"""Utils and fixtures to facilitate operations on Oneprovider web GUI.
"""
__author__ = "Jakub Liput"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


def current_dir(driver):
    return RE_DATA_URL.match(
        parse_url(driver.current_url).group('method')
    ).group('dir')


