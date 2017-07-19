"""Utils and fixtures to facilitate operations on breadcrumbs in
Oneprovider web GUI.
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


from itertools import izip
from functools import partial

from tests.gui.utils.core.base import PageObject
from tests.gui.utils.core.web_elements import WebItemsSequence, WebItem
from tests.gui.utils.core.web_objects import ButtonWithTextPageObject


class _Breadcrumbs(PageObject):
    _breadcrumbs = WebItemsSequence('a.file-breadcrumb-item-link',
                                    cls=ButtonWithTextPageObject)

    def __str__(self):
        return 'Breadcrumbs({path}) in {parent}'.format(path=self.pwd(),
                                                        parent=self.parent)

    def pwd(self):
        return '/'.join(directory.text for directory in self._breadcrumbs)

    def chdir(self, path):
        path = path.split('/')
        breadcrumbs = self._breadcrumbs
        assert len(path) <= len(breadcrumbs), \
            ('specified path {} exceeded one displayed in '
             'breadcrumbs {}'.format(path, self))

        i, dir1, dir2 = None, None, None
        err_msg = '{dir} not found on {idx}th position in {item}'

        for i, (dir1, dir2) in enumerate(izip(path, breadcrumbs)):
            assert dir1 == dir2.text, err_msg.format(dir=dir1, idx=i, item=self)

        dir2.click()


Breadcrumbs = partial(WebItem, cls=_Breadcrumbs)
