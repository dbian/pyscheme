

from typing import Dict, List, Tuple

from pyscheme.tokenizer import Token, TokenType


def define_syntax(args):
    syntax_name, transformer = args
    return ["define-syntax", syntax_name, transformer]


def syntax_rules(args):
    syntax_name, *rules = args
    return ["define-syntax", syntax_name, lambda args: expand_syntax_rules(args, rules)]


def expand_syntax_rules(args, rules):
    for pattern, template in rules:
        if match_pattern(args, pattern):
            return expand_template(args, template)
    raise SyntaxError("No matching syntax rule")


def match_pattern(args, pattern):
    if isinstance(pattern, str):
        return pattern == "_" or pattern == args
    if len(args) != len(pattern):
        return False
    return all(match_pattern(arg, pat) for arg, pat in zip(args, pattern))


def expand_template(args, template):
    if isinstance(template, str):
        return template
    if isinstance(template, list):
        return [expand_template(arg, template) for arg in args]
    return template


def macro_expand(expr: List | Token, env: Dict):
    """
    Macro expansion
    - 支持递归调用
    - 支持词法作用域
    - 支持清洁变量

    处理逻辑:
    - 记录模板，预处理成函数，用于替换时调用
    - 在调用模板的地方替换
    """
    if isinstance(expr, Tuple):
        return expr
    match expr:
        case [(TokenType.WORD, "define-syntax"), syntax_name, transformer]:
            env[syntax_name[1]] = transformer

        case [syntax_name, *args]:
            if syntax_name in env:
                transformer = env[syntax_name]
                return macro_expand(transformer(args), env)
            else:
                raise SyntaxError("Undefined syntax: " + syntax_name)
        case _:
            return expr
