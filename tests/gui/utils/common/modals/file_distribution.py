"""Utils and fixtures to facilitate operations on File distribution modal.
"""

from .modal import Modal
from tests.gui.utils.common.common import PageObject
from tests.gui.utils.common.web_elements import WebElement, \
    TextLabelWebElement, WebItemsSequence, WebItem

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class _Chunk(PageObject):
    start = TextLabelWebElement('.file-size .start')
    end = TextLabelWebElement('.file-size .end')
    _canvas = WebElement('canvas')

    def __str__(self):
        return 'file blocks for {}'.format(self.parent)

    @property
    def size(self):
        end, unit = self.end.split()
        return int(end) - int(self.start), unit

    @property
    def chunks(self):
        file_size, _ = self.size
        chunks = self.driver.execute_script(_canvas_fill, self._canvas)
        if chunks is not False:
            return [(chunk[0]*file_size, chunk[1]*file_size)
                    for chunk in chunks]
        else:
            raise RuntimeError('{} is not filled correctly: some columns '
                               'are not filled with one color'.format(self))


class _FileDistributionRecord(PageObject):
    name = id = TextLabelWebElement('.provider-name',
                                    parent_name='given provider')
    distribution = WebItem('.chunks', cls=_Chunk)

    def __str__(self):
        return 'provider record for "{item}" in ' \
               '{parent}'.format(item=self.name, parent=self.parent)


class FileDistributionModal(Modal):
    file_name = TextLabelWebElement('.modal-row strong')
    providers = WebItemsSequence('table.file-blocks-table tbody tr',
                                 cls=_FileDistributionRecord)

    def __str__(self):
        return 'File distribution modal for "{}"'.format(self.file_name)


# TODO fix not working commented code
# In case when fill color of canvas is changed,
# variable fillColor must also change to new value
_canvas_fill = """
function arraysEqual(a, b) {
    if (a === b) return true;
    if (a == null || b == null) return false;
    if (a.length != b.length) return false;

    for (var i = 0; i < a.length; ++i) {
        if (a[i] !== b[i]) return false;
    }
    return true;
}


function isCanvasFilled(cvs){
    var width = cvs.width;
    var height = cvs.height;

    var ctx = cvs.getContext("2d");
    var backgroundColor = [0, 0, 0, 0];
    var fillColor = [85, 225, 145, 255];
    var img_data = ctx.getImageData(0, 0, width, height).data;

    var idx = 0
    var pix = [];
    var refColor = backgroundColor;
    var chunk = [];
    var filled = [];

    for(var x = 0; x < width; ++x){
        idx = x * 4;
        pix = img_data.slice(idx, idx + 4);

        if(arraysEqual(pix, backgroundColor) && arraysEqual(refColor, fillColor)){
            chunk[1] = x / width;
            filled.push(chunk);
            chunk = [];
            refColor = backgroundColor;
        }
        else if(arraysEqual(pix, fillColor) && arraysEqual(refColor, backgroundColor)){
            chunk[0] = x / width;
            refColor = fillColor;
        }

        for(var y = 1; y < height; ++y){
            idx = (x + y * width) * 4;
            pix = img_data.slice(idx, idx + 4);
//            if(!arraysEqual(pix, refColor)) return false;
        }
    }
    if(arraysEqual(refColor, fillColor)){
        chunk[1] = 1;
        filled.push(chunk);
    }

    return filled;
}

return isCanvasFilled(arguments[0]);
"""
