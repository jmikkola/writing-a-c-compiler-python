class WACCException(Exception):
    pass


class SyntaxError(WACCException):
    pass


class ValidationError(WACCException):
    pass
