from tests import *
from tests.cucumber.steps import multi_auth_steps
from tests.cucumber.steps.cucumber_utils import list_parser, make_arg_list
from tests.utils.client_utils import User, mount_users
from tests.utils.docker_utils import docker_ip
from tests.utils.net_utils import http_get, http_post
from tests.utils.utils import set_dns, get_oz_cookie, get_op_cookie

from environment import docker

import json
import subprocess
from pytest_bdd import given, when, then, parsers
import tempfile


@given('openId server is started', scope="module")
def open_id(persistent_environment, env_description_file):
    oz_nodes = persistent_environment['oz_worker_nodes']
    set_dns(persistent_environment)

    open_id_host = persistent_environment['appmock_nodes'][0].split('@')[1]
    open_id_ip = docker_ip(open_id_host)

    auth_content = """[{{dropbox, [
            {{auth_module, auth_dropbox}},
            {{app_id, <<"mock_id">>}},
            {{app_secret, <<"mock_secret">>}},
            % Provider specific config
            {{authorize_endpoint, <<"https://{open_id_ip}/1/oauth2/authorize">>}},
            {{access_token_endpoint, <<"https://{open_id_ip}/1/oauth2/token">>}},
            {{user_info_endpoint, <<"https://{open_id_ip}/1/account/info">>}}]
        }}].""".format(open_id_ip=open_id_ip)

    CONFIG_PATH = os.path.join('/root', 'bin', 'node', 'data', 'auth.config')

    # tmp_dir = tempfile.mkdtemp()
    # tmp_file = tempfile.mktemp(dir=tmp_dir)
    tmp_file = tempfile.mktemp()
    # docker.cp(oz_host, CONFIG_PATH, tmp_dir, False)

    with open(tmp_file, 'w') as f:
        f.write(auth_content)

    for oz_node in oz_nodes:
        oz_host = oz_node.split('@')[1]
        cookie = get_oz_cookie(env_description_file, oz_node)

        docker.cp(oz_host, tmp_file, CONFIG_PATH, True)

        subprocess.check_output(
                [os.path.join(UTILS_DIR, 'load_auth_config.escript'),
                 oz_node, cookie], stderr=subprocess.STDOUT)

    os.remove(tmp_file)
    return open_id_ip


@given(parsers.parse('there are users {users} with emails\n{emails}'))
def save_users(users, emails, context):
    users = list_parser(users)
    emails = list_parser(emails)
    if not hasattr(context, "users"):
        context.users = {}
    for user, email in zip(users, emails):
        if user not in context.users:
            context.users[user] = User(email=email)
        else:
            context.users[user].email = email
    return users


@given('users are logged in', scope="module")
def login(persistent_environment, open_id, context):
    """Note number of users cannot be grater than number of users mocked in
    open id server appmoc
    """
    users = context.users.keys()
    oz_nodes = persistent_environment['oz_worker_nodes']
    oz_node = oz_nodes[0]
    oz_host = oz_node.split('@')[1]

    op_nodes = persistent_environment['op_worker_nodes']
    op_node = op_nodes[0]
    op_host = op_node.split('@')[1]

    oz_ip = docker_ip(oz_host)
    context.oz_ip = oz_ip
    context.oz_domain = ".".join(oz_host.split('.')[1::])

    op_ip = docker_ip(op_host)
    context.op_ip = op_ip
    context.op_domain = ".".join(op_host.split('.')[1::])

    for user in users:
        http_get(oz_ip, 443, "/do_login?provider=dropbox", True)


@given(parsers.parse('users are authorized'), scope="module")
def get_macaroon(request, persistent_environment, env_description_file, open_id,
                 context):
    users = context.users.keys()
    oz_nodes = persistent_environment['oz_worker_nodes']
    oz_node = oz_nodes[0]
    oz_cookie = get_oz_cookie(env_description_file, oz_node)

    op_nodes = persistent_environment['op_worker_nodes']
    op_node = op_nodes[0]
    op_cookie = get_op_cookie(env_description_file, op_node)

    key_file = os.path.join(os.path.dirname(__file__), "key.pem")
    cert_file = os.path.join(os.path.dirname(__file__), "cert.pem")
    csr_file = os.path.join(os.path.dirname(__file__), "csr.pem")
    context.key_file = key_file
    context.cert_file = cert_file

    # TODO move this function to utils
    subprocess.call(['openssl', 'genrsa', '-out', key_file, '2048'])
    subprocess.call(
            ['openssl', 'req', '-new', '-batch', '-key', key_file, '-out',
             csr_file])

    with open(csr_file, 'r') as csr:
        csr_content = csr.read()

    _, _, cert_content = http_post(context.oz_domain, REST_PORT, "/provider",
                                   True,
                                   data=json.dumps({
                                       'csr': csr_content,
                                       'clientName': "test_provider",
                                       'redirectionPoint': "",
                                       'urls': ["{url}:{port}"
                                                    .format(
                                           url=context.op_domain, port=443)]}),
                                   headers=DEFAULT_HEADERS)

    with open(cert_file, 'w') as cert:
        cert.write(json.loads(cert_content)['certificate'])

    for user in users:
        user = context.get_user(user)
        headers_str = subprocess \
            .check_output([os.path.join(UTILS_DIR, 'get_macaroon.escript'),
                           oz_node, oz_cookie, op_node, op_cookie,
                           "{}".format(user.email),
                           "p1"],
                          stderr=subprocess.STDOUT)  # TODO provider id is hardcoded
        auth_headers = json.loads(headers_str.split('\n')[-1])
        user.headers = dict(DEFAULT_HEADERS)
        user.headers.update(auth_headers)
        print "HEADERS: ", user.headers

    def fin():
        os.remove(key_file)
        os.remove(cert_file)
        os.remove(csr_file)

    request.addfinalizer(fin)


@given('users info is saved', scope="module")
def save_info(context):
    users = context.users.keys()
    for user in users:
        user = context.get_user(user)

        _, _, body = http_get(context.oz_domain, REST_PORT, "/user", True,
                              headers=user.headers,
                              cert=(context.cert_file, context.key_file))

        user.id = json.loads(body)['userId']


# TODO this step is slightly different than one in auth_steps
# TODO difference is in list of ids
# TODO find way to use ids in general step (now user name is used as id)
# TODO maybe we should implement users and clients fixtures that would
# TODO be maps of suitable objects, initialized basing on env_up ouptut
@given(parsers.parse(
        '{user} starts oneclient in {mount_path} using {token} on {client_node}'))
def mount(user, mount_path, token, client_node, request, environment, context,
          client_ids,
          env_description_file):
    mount_users(request, environment, context, client_ids, env_description_file,
                users=[user], ids=[context.get_user(user).id],
                client_instances=[client_node], mount_paths=[mount_path],
                client_hosts=['client-host1'], tokens=[token], check=False)


@given(parsers.parse('{users} start oneclients {client_instances} in\n' +
                     '{mount_paths} on client_hosts\n' +
                     '{client_hosts} respectively,\n' +
                     'using {tokens}'))
def multi_mount(users, client_instances, mount_paths, client_hosts, tokens,
                request, environment, context, client_ids,
                env_description_file):
    ids = [context.get_user(user).id for user in list_parser(users)]

    mount_users(request, environment, context, client_ids,
                env_description_file, users=list_parser(users), ids=ids,
                client_instances=list_parser(client_instances),
                mount_paths=list_parser(mount_paths),
                client_hosts=list_parser(client_hosts),
                tokens=list_parser(tokens))


@when(parsers.parse('{spaces} is mounted for {user} on {client_node}'))
@then(parsers.parse('{spaces} is mounted for {user} on {client_node}'))
@when(parsers.parse('{spaces} are mounted for {user} on {client_node}'))
@then(parsers.parse('{spaces} are mounted for {user} on {client_node}'))
def check_spaces(spaces, user, client_node, context):
    multi_auth_steps.check_spaces(spaces, user, make_arg_list(client_node),
                                  context)
