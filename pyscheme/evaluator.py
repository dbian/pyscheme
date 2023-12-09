import operator
import math
from pyscheme.tokenizer import *
from typing import Dict, Generator, List, Tuple
from . import tokenizer


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


# 定义全局环境
global_env = {
    "+": operator.add,
    "-": operator.sub,
    "*": operator.mul,
    "/": operator.truediv,
    "mod": operator.mod,
    "expt": math.pow,
    ">": operator.gt,
    "<": operator.lt,
    ">=": operator.ge,
    "<=": operator.le,
    "=": operator.eq,
    "abs": abs,
    "append": operator.add,
    "not": operator.not_,
    "null?": lambda x: x is None,
    "list?": lambda x: isinstance(x, list),
    "symbol?": lambda x: isinstance(x, str),
    "number?": lambda x: isinstance(x, (int, float)),
    "car": lambda x: x[0] if x else None,
    "cdr": lambda x: x[1:] if x else None,
    "list": lambda *x: list(x),
    "map": lambda x, y: list(map(x, y)),
    "define-syntax": define_syntax,
    "syntax-rules": syntax_rules
}


def get_list_val(lst):
    return [x[1] for x in lst]

# 定义解释器的函数


def eval_sexpression(expr: List | Token, env: Dict):
    # expr = macro_expand(expr, env)  # Macro expansion
    if not isinstance(expr, list):
        # print('eval sg', expr)
        kind, val = expr
        match kind:
            case TokenType.STRING | TokenType.NUMBER:
                return val
            case TokenType.WORD:
                if val not in env:
                    raise NameError("Undefined symbol: " + val)  # type: ignore
                return env[val]
            case _:
                raise SyntaxError(f"Invalid syntax: {expr}")
    match [expr[0][1], *expr[1:]]:
        case ["cond", *clauses]:
            for clause in clauses:
                if eval_sexpression(clause[0], env):
                    return eval_sexpression(clause[1], env)
            return None
        case ["list-tail", lst, k]:
            lst = eval_sexpression(lst, env)
            k = eval_sexpression(k, env)
            return lst[k:]
        case ["quote", exp]:
            return exp

        case ["if", test, conseq, alt]:
            exp = conseq if eval_sexpression(test, env) else alt
            return eval_sexpression(exp, env)
        case ["define", [(TokenType.WORD, func_name), *params], body]:
            env[func_name] = lambda *args: eval_sexpression(
                body, env | dict(zip(get_list_val(params), args)))
        case ["define", symbol, exp]:
            env[symbol[1]] = eval_sexpression(exp, env)
        case ["lambda", params, body]:
            return lambda *args: eval_sexpression(body, env | dict(zip(get_list_val(params), args)))
        case ["begin", *exps]:
            result = None
            for exp in exps:
                result = eval_sexpression(exp, env)
            return result
        case ["let", bindings, *body]:
            new_env = env.copy()
            for binding in bindings:
                symbol, exp = binding
                value = eval_sexpression(exp, env)
                new_env[symbol[1]] = value
            return eval_sexpression([(TokenType.WORD, "begin"), *body], new_env)
        case ["let*", bindings, *body]:
            new_env = env.copy()
            for binding in bindings:
                symbol, exp = binding
                value = eval_sexpression(exp, new_env)  # 使用new_env来求值
                new_env[symbol[1]] = value
            return eval_sexpression([(TokenType.WORD, "begin"), *body], new_env)
        case [proc, *args]:
            proc = eval_sexpression(expr[0], env)
            args = [eval_sexpression(arg, env) for arg in args]
            return proc(*args)
        case _:
            raise SyntaxError("Invalid syntax")


def macro_expand(expr, env):
    match expr:
        case expr if isinstance(expr, str):
            if expr in env:
                return env[expr]
            if expr.startswith('"') and expr.endswith('"'):
                return expr
            raise NameError("Undefined symbol: " + expr)

        case expr if not isinstance(expr, list):
            return expr

        case ["define-syntax", syntax_name, transformer]:
            env[syntax_name] = transformer

        case [syntax_name, *args]:
            if syntax_name in env:
                transformer = env[syntax_name]
                return macro_expand(transformer(args), env)
            else:
                raise SyntaxError("Undefined syntax: " + syntax_name)

        case _:
            raise SyntaxError("Invalid syntax")


# 安装函数到解释器环境


def put(name, func, env=None):
    if env:
        env[name] = func
        return
    global global_env
    global_env[name] = func


def run_yield(code: str, env: dict) -> Generator:
    tokens = tokenizer.tokenize(code)

    while len(tokens) > 0:
        exprs = parse(tokens)
        res = eval_sexpression(exprs, env)
        if isinstance(res, Generator):
            yield from res
        else:
            yield res

# 运行字符串源码


def run(code: str, env: dict):
    res = None
    for x in run_yield(code, env):
        res = x
    return res

# 运行文件


def run_file(filename, env):
    with open(filename, "r", encoding="utf-8") as file:
        code = file.read()
        run(code, env)


def run_file_yield(filename, env) -> Generator:
    with open(filename, "r", encoding="utf-8") as file:
        code = file.read()
        yield from run_yield(
            code, env)

# 解析


def parse(tokens: List) -> List[Token] | Token:
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
        case _:
            return v


def new_env():
    return global_env.copy()


# 创建交互式命令行界面
def repl(env=None):
    env_new = env or global_env.copy()
    while True:
        try:
            code = input("Scheme> ")
            res = run(code, env_new)
            env_new["*3"] = env_new["*2"] if "*2" in env_new else None
            env_new["*2"] = env_new["*1"] if "*1" in env_new else None
            env_new["*1"] = res
            if res is not None:
                print(res)
        except (EOFError, KeyboardInterrupt):
            print("\nGoodbye!")
            break
        except Exception as e:
            print("Error:", str(e))


def display(x):
    print(x)


put("display", display)

# python bridge
put("attr", getattr)

if __name__ == "__main__":
    # 启动交互式命令行界面
    repl()
