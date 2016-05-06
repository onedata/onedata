"""This module contains util functions for git"""
import os
import subprocess


def get_branch_name():
    return subprocess.check_output(["git", "rev-parse", "--abbrev-ref", "HEAD"])


def get_commit():
    return subprocess.check_output(["git", "rev-parse", "HEAD"])


def get_repository():
    toplevel = subprocess.check_output(['git', 'rev-parse', '--show-toplevel'])
    return os.path.basename(toplevel).strip()