

from typing import Any, List
from pyscheme.tokenizer import Token, TokenType


def parse(tokens: List) -> List[Any] | Token:
    if len(tokens) == 0:
        raise SyntaxError("Unexpected EOF")

    v = tokens.pop(0)
    _, token = v
    match token:
        case "(":
            expr = []
            while tokens[0][1] != ")":
                expr.append(parse(tokens))
            tokens.pop(0)  # 弹出')'
            return expr
        case ")":
            raise SyntaxError("Unexpected )")
        case "'" | "`":
            return [(TokenType.WORD, "quote"), parse(tokens)]
        case ",":
            return [(TokenType.WORD, "unquote"), parse(tokens)]
        case ",@":
            return [(TokenType.WORD, "unquote-splicing"), parse(tokens)]
        case _:
            return v
