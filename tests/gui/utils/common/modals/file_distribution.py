"""Utils and fixtures to facilitate operations on File distribution modal.
"""

from tests.gui.utils.common.common import PageObject
from tests.gui.utils.common.modals.modal import Modal
from tests.gui.utils.common.web_elements import WebElement, TextLabelWebElement, ItemListWebElement

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


class FileDistributionModal(Modal):
    file_name = TextLabelWebElement('.modal-row strong')
    _providers = ItemListWebElement('table.file-blocks-table tbody tr')

    def __str__(self):
        return 'File distribution modal for "{}"'.format(self.file_name)

    def __iter__(self):
        return (FileDistributionRecord(self.driver, prov, self)
                for prov in self._providers)

    def __getitem__(self, name):
        for provider_record in self:
            if provider_record.provider == name:
                return provider_record
        else:
            raise RuntimeError('no record for provider named "{}" '
                               'found in {}'.format(name, self))


class FileDistributionRecord(PageObject):
    provider = TextLabelWebElement('.provider-name',
                                   parent_name='given provider')
    _distribution = WebElement('.chunks')

    def __str__(self):
        return 'file distribution record for {0} provider in ' \
               '{1}'.format(self.provider, self.parent)

    @property
    def distribution(self):
        return Chunk(self.driver, self._distribution, self)


class Chunk(PageObject):
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
    var strokeColor = [0, 0, 0, 0];
    var fillColor = [85, 225, 145, 255];
    var img = ctx.getImageData(0, 0, width, height).data;

    var idx = 0
    var pix = [];
    var refColor = strokeColor;
    var chunk = [];
    var filled = [];

    for(var i = 0; i < width; ++i){
        idx = i * 4;
        pix = img.slice(idx, idx + 4);

        if(arraysEqual(pix, strokeColor) && arraysEqual(refColor, fillColor)){
            chunk[1] = i / width;
            filled.push(chunk);
            chunk = [];
            refColor = strokeColor;
        }
        else if(arraysEqual(pix, fillColor) && arraysEqual(refColor, strokeColor)){
            chunk[0] = i / width;
            refColor = fillColor;
        }

        for(var j = 1; j < height; ++j){
            idx = (i + j * width) * 4;
            pix = img.slice(idx, idx + 4);
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
