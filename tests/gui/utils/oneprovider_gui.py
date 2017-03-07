"""Utils and fixtures to facilitate operations on Oneprovider web GUI.
"""

from tests.gui.utils.common.web_elements import WebElement
from tests.gui.utils.generic import find_web_elem_with_text, click_on_web_elem
from tests.gui.utils.oneprovider.data_tab import DataTab
from tests.gui.utils.oneprovider.user_profile import UserProfile

__author__ = "Jakub Liput, Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


def assert_breadcrumbs_correctness(path, breadcrumbs):
    cwd = breadcrumbs.text.split()
    path = path.split('/')
    err_msg = '{} not found on {} position in breadcrumbs, instead we have {}'
    assert len(path) == len(cwd), 'found {} path in breadcrumbs ' \
                                  'instead of {}'.format(cwd, path)
    for i, (dir1, dir2) in enumerate(zip(path, cwd)):
        assert dir1 == dir2, err_msg.format(dir1, i, '/'.join(cwd))


def chdir_using_breadcrumbs(path, breadcrumbs):
    dir1, dir2 = None, None
    err_msg = '{} not found on {} position in breadcrumbs, instead we have {}'
    for i, (dir1, dir2) in enumerate(zip(path.split('/'), breadcrumbs)):
        assert dir1 == dir2.text, err_msg.format(dir1, i,
                                                 '/'.join(directory.text
                                                          for directory
                                                          in breadcrumbs)
                                                 )
    dir2.click()


# this function was blocking import of this module because of errors
# def current_dir(driver):
#     return RE_DATA_URL.match(
#         parse_url(driver.current_url).group('method')
#     ).group('dir')


class OPLoggedIn(object):
    _user_profile = WebElement('li.profile-dropdown')

    tabs = {'data': DataTab}

    def __init__(self, driver):
        self.web_elem = driver

    def __str__(self):
        return 'Oneprovider page'

    def __getattr__(self, item):
        return self.tabs[item](self.web_elem, self.web_elem, self)

    @property
    def user_profile(self):
        return UserProfile(self.web_elem, self._user_profile, self)

    def click_on_tab(self, name):
        css_sel = 'nav.primary-sidebar li a'
        err_msg = '{} btn in {} not found'.format(name, self)
        btn = find_web_elem_with_text(self.web_elem, css_sel,
                                      name.lower(), err_msg)

        err_msg = 'cannot click on {} in {}'.format(name, self)
        click_on_web_elem(self.web_elem, btn, err_msg)
