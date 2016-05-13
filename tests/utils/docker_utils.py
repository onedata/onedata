import subprocess

from environment import docker


def run_cmd(user, client, cmd, output=False, error=False):
    # convert command into ascii string or list of ascii strings
    if isinstance(cmd, basestring):
        cmd = str(cmd)
    elif isinstance(cmd, list):
        cmd = [str(x) for x in cmd]

    if user != 'root' and isinstance(cmd, str):
        cmd = ['su', '-c', cmd, str(user)]
    elif user != 'root' and isinstance(cmd, list):
        cmd = ["su", "-c"] + cmd + [str(user)]

    return docker.exec_(container=client.docker_id, command=cmd, output=output,
                        tty=True, stderr=subprocess.STDOUT if error else None)
