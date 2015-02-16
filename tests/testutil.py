import inspect
import py.process


# Returns a path to file located in {test_name}_data directory, where
# {test_name} is name of the test module that called this function.
# example: using tesutils.test_file('my_file') in my_test.py will return 'tests/my_test_data/my_file'
def test_file(relative_file_path):
    caller = inspect.stack()[1]
    caller_mod = inspect.getmodule(caller[0])
    caller_mod_file_path = caller_mod.__file__
    return '{0}_data/{1}'.format(caller_mod_file_path.rstrip('.py'), relative_file_path)


# Runs a given command and returns unicode output.
# The argument may be:
# 1) a full command: 'ls -al'
# 2) list of strings to be joined with spaces: ['ls' '-al']
def run_command(string_or_list):
    if isinstance(string_or_list, list):
        string_or_list = ' '.join(string_or_list)
    elif not isinstance(string_or_list, str):
        raise ValueError("argument must be a string or a list of strings")
    # Execute the command and remove trailing whitespaces
    return py.process.cmdexec(string_or_list).rstrip()

