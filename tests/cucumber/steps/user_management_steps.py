import json
import os
import subprocess

from pytest_bdd import given, when, then, parsers

from tests import UTILS_DIR
from tests.utils.docker_utils import docker_ip
from tests.utils.net_utils import http_get, http_post
from tests.utils.utils import get_op_cookie, get_oz_cookie


@given('openId server is started')
def configure_openid(open_id, environment):
    print "CONFIGURE: "


@given(parsers.parse('{user} is logged in'))
def login(user, environment, persistent_environment, open_id):
    oz_nodes = persistent_environment['oz_worker_nodes']
    oz_node = oz_nodes[0]
    oz_host = oz_node.split('@')[1]
    oz_ip = docker_ip(oz_host)
    response = http_get(oz_ip, 443, "/do_login?provider=dropbox", True)
    print "RESPONSE: ", response


@given(parsers.parse('{user} has macaroon'))
def get_macaroon(user, persistent_environment, env_description_file, environment, open_id):

    oz_nodes = persistent_environment['oz_worker_nodes']
    oz_node = oz_nodes[0]
    oz_host = oz_node.split('@')[1]

    oz_cookie = get_oz_cookie(env_description_file, oz_node)

    op_nodes = persistent_environment['op_worker_nodes']
    op_node = op_nodes[0]
    op_host = op_node.split('@')[1]

    op_cookie = get_op_cookie(env_description_file, op_node)

    headers_str = subprocess.check_output([os.path.join(UTILS_DIR, 'get_macaroon.escript'),
                                           oz_node, oz_cookie, op_node,
                                           op_cookie, "{}@mail.com".format(user),
                                           "p1"], stderr=subprocess.STDOUT)

    headers = json.loads(headers_str)





@when(parsers.parse('{user} registers in onedata'))
def register_user(user, context, environment):
    print "register_user"


@when(parsers.parse('{user} is registered'))
def is_user_registered(user, context, environment):
    print "is_user_registered"

