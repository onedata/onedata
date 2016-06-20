#TODO DOC
import re


def parse(regex, string, *groups):
    if groups:
        pattern = re.compile(regex)
        m = re.match(pattern, string)
        if len(groups) > 1:
            out = []
            for i in groups:
                out.append(m.group(i))
            return tuple(out)
        else:
            return m.group(1)