import re

RE_URL = re.compile(r'(?P<base_url>https?://(?P<domain>.*?))(?P<method>/.*)')


def parse_url(url):
    return RE_URL.match(url)