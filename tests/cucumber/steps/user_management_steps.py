from pytest_bdd import given, when, then, parsers


@given('openId server is started')
def configure_openid(open_id, environment):
    print "CONFIGURE: "


@when(parsers.parse('{user} registers in onedata'))
def register_user(user, context, environment):
    print "register_user"


@when(parsers.parse('{user} is registered'))
def is_user_registered(user, context, environment):
    print "is_user_registered"
