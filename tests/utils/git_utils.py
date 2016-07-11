"""This module contains utility functions for git
"""
__author__ = "Konrad Zemek, Jakub Kudzia"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"
import os
import subprocess


def get_branch_name():
    return subprocess.check_output(["git", "rev-parse", "--abbrev-ref", "HEAD"])


def get_commit():
    return subprocess.check_output(["git", "rev-parse", "HEAD"])


def get_repository():
    toplevel = subprocess.check_output(['git', 'rev-parse', '--show-toplevel'])
    return os.path.basename(toplevel).strip()
