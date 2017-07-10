"""Exceptions classes for http errors.
"""

__author__ = "Bartek Walkowicz"
__copyright__ = "Copyright (C) 2017 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"


_exceptions = {}


def checked_call(call_fun, *args, **kwargs):
    response = call_fun(*args, **kwargs)
    if 200 <= response.status_code < 300:
        return response
    else:
        ex_cls = _exceptions.get(response.status_code, HTTPError)
        raise ex_cls(response)


class HTTPErrorMeta(type):
    def __new__(cls, *args, **kwargs):
        new_cls = super(HTTPErrorMeta, cls).__new__(cls, *args, **kwargs)
        if hasattr(new_cls, 'status_code'):
            _exceptions[new_cls.status_code] = new_cls
        return new_cls


class HTTPError(IOError):
    __metaclass__ = HTTPErrorMeta

    def __init__(self, response):
        self.response = response

    def __str__(self):
        return '[{status}] {reason}: {text}'.format(
            status=self.response.status_code,
            reason=self.response.reason,
            text=self.response.text
        )


class HTTPClientError(HTTPError):
    """4xx responses."""


class HTTPServerError(HTTPError):
    """5xx responses."""


class HTTPBadRequest(HTTPClientError):
    status_code = 400


class HTTPUnauthorized(HTTPClientError):
    status_code = 401


class HTTPPaymentRequired(HTTPClientError):
    status_code = 402


class HTTPForbidden(HTTPClientError):
    status_code = 403


class HTTPNotFound(HTTPClientError):
    status_code = 404


class HTTPMethodNotAllowed(HTTPClientError):
    status_code = 405


class HTTPNotAcceptable(HTTPClientError):
    status_code = 406


class HTTPProxyAuthenticationRequired(HTTPClientError):
    status_code = 407


class HTTPRequestTimeout(HTTPClientError):
    status_code = 408


class HTTPConflict(HTTPClientError):
    status_code = 409


class HTTPGone(HTTPClientError):
    status_code = 410


class HTTPLengthRequired(HTTPClientError):
    status_code = 411


class HTTPPreconditionFailed(HTTPClientError):
    status_code = 412


class HTTPPayloadTooLarge(HTTPClientError):
    status_code = 413


class HTTPURITooLong(HTTPClientError):
    status_code = 414


class HTTPUnsupportedMediaType(HTTPClientError):
    status_code = 415


class HTTPRangeNotSatisfiable(HTTPClientError):
    status_code = 416


class HTTPExpectationFailed(HTTPClientError):
    status_code = 417


class HTTPMisdirectedRequest(HTTPClientError):
    status_code = 421


class HTTPUnprocessableEntity(HTTPClientError):
    status_code = 422


class HTTPLocked(HTTPClientError):
    status_code = 423


class HTTPFailedDependency(HTTPClientError):
    status_code = 424


class HTTPUpgradeRequired(HTTPClientError):
    status_code = 426


class HTTPPreconditionRequired(HTTPClientError):
    status_code = 428


class HTTPTooManyRequests(HTTPClientError):
    status_code = 429


class HTTPRequestHeaderFieldsTooLarge(HTTPClientError):
    status_code = 431


class HTTPUnavailableForLegalReasons(HTTPClientError):
    status_code = 451


class HTTPInternalServerError(HTTPServerError):
    status_code = 500


class HTTPNotImplemented(HTTPServerError):
    status_code = 501


class HTTPBadGateway(HTTPServerError):
    status_code = 502


class HTTPServiceUnavailable(HTTPServerError):
    status_code = 503


class HTTPGatewayTimeout(HTTPServerError):
    status_code = 504


class HTTPHTTPVersionNotSupported(HTTPServerError):
    status_code = 505


class HTTPVariantAlsoNegotiates(HTTPServerError):
    status_code = 506


class HTTPInsufficientStorage(HTTPServerError):
    status_code = 507


class HTTPLoopDetected(HTTPServerError):
    status_code = 508


class HTTPNotExtended(HTTPServerError):
    status_code = 510


class HTTPNetworkAuthenticationRequired(HTTPServerError):
    status_code = 511
