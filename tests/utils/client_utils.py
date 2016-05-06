from tests.utils.docker_utils import run_cmd


class User:
    def __init__(self, client_node, client):
        self.clients = {client_node: client}
        self.last_op_ret_code = 0
        self.files = {}


class Client:
    def __init__(self, docker_id, mount_path):
        self.timeout = 0
        self.docker_id = docker_id
        self.mount_path = mount_path

    def set_timeout(self, timeout):
        self.timeout = timeout


def ls(client, user="root", path=".", output=True):
    cmd = "ls {path}".format(path=path)
    return run_cmd(user, client, cmd, output=output)


def mv(client, src, dest, user="root", output=False):
    cmd = "mv {src} {dest}".format(src=src, dest=dest)
    return run_cmd(user, client, cmd, output=output)


def chmod(client, mode, file, user="root", output=False):
    cmd = "chmod {mode} {file}".format(mode=mode, file=file)
    return run_cmd(user, client, cmd, output=output)


def stat(client, path, format=None, user="root", output=True):
    cmd = "stat {path} {format}".format(
            path=path,
            format="--format='{0}'".format(format) if format else "")
    return run_cmd(user, client, cmd, output=output)


def rm(client, path, recursive=False, force=False, user="root", output=False):
    cmd = "rm {recursive} {force} {path}".format(
            recursive="-r" if recursive else "",
            force="-f" if force else "",
            path=path)
    return run_cmd(user, client, cmd, output=output)


def rmdir(client, dir_path, recursive=False, from_path=None, user="root",
          output=False):
    cmd = ("{from_path}"
           "rmdir {recursive} {path}"
           ).format(
            from_path="cd {0} &&".format(from_path) if from_path else "",
            recursive="-p" if recursive else "",
            path=dir_path)
    return run_cmd(user, client, cmd, output=output)


def mkdir(client, dir_path, recursive=False, user="root", output=False):
    cmd = "mkdir {recursive} {path}".format(
            recursive="-p" if recursive else "",
            path=dir_path)
    return run_cmd(user, client, cmd, output=output)


def touch(client, file_path, user="root", output=False):
    cmd = "touch {path}".format(path=file_path)
    return run_cmd(user, client, cmd, output=output)


def cp(client, src, dest, recursive=False, user="root", output=False):
    cmd = "cp {recursive} {src} {dest}".format(
            recursive="-r" if recursive else "",
            src=src,
            dest=dest)
    return run_cmd(user, client, cmd, output=output)


def truncate(client, file_path, size, user="root", output=False):
    cmd = "truncate --size={size} {file_path}".format(size=size, file_path=file_path)
    return run_cmd(user, client, cmd, output=output)


def dd(client, block_size, count, output_file, input_file="/dev/zero",
       user="root", output=True):

    cmd = "dd {input} {output} {bs} {count}".format(
        input="if={}".format(input_file),
        output="of={}".format(output_file),
        bs="bs={}M".format(block_size),  # block_size is in MB
        count="count={}".format(count))
    return run_cmd(user, client, cmd, output=output)


def echo_to_file(client, text, file_path, new_line=False, escape=False,
                 user="root", overwrite=True, output=False):

    cmd = "echo {newline} {escape} '{text}' {redirect} {file_path}".format(
        newline="-n" if not new_line else "",
        escape="-e" if escape else "",
        text=text,
        redirect=">" if overwrite else ">>",
        file_path=file_path)
    return run_cmd(user, client, cmd, output=output)


def cat(client, file_path, user="root", output=True):
    cmd = "cat {file_path}".format(file_path=file_path)
    return run_cmd(user, client, cmd, output=output)


def md5sum(client, file_path, user="root", output=True):
    cmd = "md5sum {file_path}".format(file_path=file_path)
    return run_cmd(user, client, cmd, output=output)


def replace_pattern(client, file_path, pattern, new_text, user='root',
                    output=False):
    cmd = 'sed -i \'s/{pattern}/{new_text}/g\' {file_path}'.format(
            pattern=pattern,
            new_text=new_text,
            file_path=file_path)
    return run_cmd(user, client, cmd, output=output)


def fusermount(client, path, user='root', unmount=False, lazy=False,
               quiet=False, output=False):
    cmd = "fusermount {unmount} {lazy} {quiet} {path}".format(
        unmount="-u" if unmount else "",
        lazy="-z" if lazy else "",
        quiet="-q" if quiet else "",
        path=path
    )
    return run_cmd(user, client, cmd, output)