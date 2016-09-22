"""Helper objects and functions mimicking file system in Oneprovider
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


import collections


OneDirectory = collections.namedtuple('OneDirectory', ['type', 'name', 'files'])
OneRegularFile = collections.namedtuple('OneRegularFile', ['type', 'name', 'dir'])
OneShare = collections.namedtuple('OneShare', ['type', 'name', 'shared'])


def mkdir(name, in_dir=None):
    directory = OneDirectory(type=OneDirectory, name=name, files={})
    if in_dir:
        if name in in_dir:
            raise ValueError('{:s}/{:s} already exist'.format(in_dir.name,
                                                              name))
        else:
            in_dir.files[name] = directory

    directory.files.update({'.': directory,
                            '..': in_dir})
    return directory


def rmdir(name, in_dir):
    in_dir.files.pop(name)


def touch(name, in_dir=None):
    regular_file = OneRegularFile(type=OneRegularFile, name=name, dir=in_dir)
    if in_dir:
        if name in in_dir:
            raise ValueError('{:s}/{:s} already exist'.format(in_dir.name,
                                                              name))
        else:
            in_dir.files[name] = regular_file
    return regular_file


def rmfile(name, in_dir):
    in_dir.files.pop(name)


def mkshare(browser, name, item, tmp_memory):
    share = OneShare(type=OneShare, name=name, shared=item)
    if name in tmp_memory[browser]['shares']:
        raise ValueError('share {:s} already exist')
    else:
        tmp_memory[browser]['shares'][name] = share


def rmshare(browser, name, in_dir, tmp_memory):
    share = tmp_memory[browser]['shares'][name]
    if share.shared:
        (rmdir if share.shared == OneDirectory
         else rmfile)(share.shared.name, in_dir)
    tmp_memory[browser]['shares'].pop(name)


def _get_dir_path(directory, relative_root):
    if directory.name == relative_root or not directory.files['..']:
        return [directory.name]
    else:
        path = _get_dir_path(directory.files['..'], relative_root)
        path.append(directory.name)
        return path


def get_path(item, relative_root=None):
    if item.type == OneDirectory:
        return _get_dir_path(item, relative_root)
    elif item.type == OneRegularFile:
        path = _get_dir_path(item.dir, relative_root)
        path.append(item.name)
        return path
    else:
        raise ValueError('not handled item type {:s}'.format(item.type))


def ls(directory):
    return {file_name: file.type for file_name, file
            in directory.files.iteritems()
            if file_name not in ('.', '..')}
