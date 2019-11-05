#!/usr/bin/env python
# -*- coding: utf-8 -*-

from os.path import join
import os
import sys
import shutil

PERSISTENCE_DIR = '/volumes/persistence'
BACKUP_DIR = '/volumes/persistence_backup'
DIRS = [
    '/etc/op_panel',
    '/etc/op_worker',
    '/etc/cluster_manager',
    '/etc/ceph',
    '/var/lib/op_panel/mnesia',
    '/var/log/op_panel',
    '/var/log/op_worker',
    '/var/log/cluster_manager',
    '/opt/couchbase/var/lib/couchbase',
    '/var/lib/ceph',
    '/var/log/ceph'
]


def create_symlinks():
    if not os.path.isdir(PERSISTENCE_DIR):
        os.makedirs(PERSISTENCE_DIR)

    for dest_path in DIRS:
        source_path = join(PERSISTENCE_DIR, dest_path[1:])

        if not os.path.isdir(source_path):
            stat = os.stat(dest_path)
            os.makedirs(source_path)
            os.chown(source_path, stat.st_uid, stat.st_gid)

        if not os.path.islink(dest_path):
            shutil.rmtree(dest_path)
            os.symlink(source_path, dest_path)


def backup_persistent_files():
    if not os.path.isdir(BACKUP_DIR):
        os.makedirs(BACKUP_DIR)
    copy_missing_files(None, BACKUP_DIR)


# src can be None if there is no base dir
def copy_missing_files(base_dir, dest):
    for root_dir in DIRS:
        if base_dir:
            root_dir = join(base_dir, root_dir[1:])

        if not os.path.exists(root_dir):
            os.makedirs(root_dir)

        for subdir, _, files in os.walk(root_dir):
            if base_dir:
                subdir_path = join(dest, os.path.relpath(subdir, base_dir))
            else:
                subdir_path = join(dest, subdir[1:])

            if not os.path.exists(subdir_path):
                stat = os.stat(subdir)
                os.makedirs(subdir_path)
                os.chown(subdir_path, stat.st_uid, stat.st_gid)

            for f in files:
                source_path = join(subdir, f)
                dest_path = join(subdir_path, f)
                if not os.path.exists(dest_path):
                    stat = os.stat(source_path)
                    shutil.copy(source_path, dest_path)
                    os.chown(dest_path, stat.st_uid, stat.st_gid)


if __name__ == '__main__':
    if sys.argv[1] == '--create-symlinks':
        print('Creating symlinks to persistence dir...')
        create_symlinks()
    elif sys.argv[1] == '--backup-persistent-files':
        print('Backing-up persistent files...')
        backup_persistent_files()
    elif sys.argv[1] == '--copy-missing-files':
        print('Copying missing persistent files...')
        copy_missing_files(BACKUP_DIR, PERSISTENCE_DIR)
    else:
        sys.exit(1)
    print('Done.')
