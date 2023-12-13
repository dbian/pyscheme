import os
import operator
import math
from pyscheme.parser import parse
from pyscheme.tokenizer import *
from typing import Any, Dict, Generator, List, Tuple
from . import tokenizer
import functools


def define_syntax(expr, env):
    if len(expr) != 3 or expr[1][0] != TokenType.WORD:
        raise SyntaxError("Invalid syntax: define-syntax")

    macro_name = expr[1][1]
    transformer = eval_sexpression(expr[2], env)
    env[macro_name] = ("macro", transformer)


def syntax_rules(literals, pattern: List[Token]):
    patterns, templates = zip(*pattern)
    if len(patterns) != len(templates):
        raise SyntaxError("Number of patterns must match number of templates")

    def match_pattern(pattern: List[Token], expr: List[Any]):
        def match_pattern_r(pattern: List[Token], expr: List[Any], bindings: Dict[str, Any] = {}):
            if not pattern and not expr:
                if bindings:
                    return ("binding", bindings)
                return ("success",)
            elif not pattern or not expr:
                return None
            if len(pattern) == 2:
                match (pattern[0], pattern[1]):
                    case ((TokenType.WORD, _), (TokenType.WORD, "...")):
                        if pattern[0][1] in bindings:
                            raise SyntaxError(
                                f"Duplicate binding: {pattern[0][1]}")
                        bindings[(pattern[0], pattern[1])] = expr  # 特殊处理，列表形式
                        return ('binding', bindings)

            if pattern[0][0] == TokenType.WORD and pattern[0][1] == "_":
                return match_pattern_r(pattern[1:], expr[1:], bindings)
            elif pattern[0] == expr[0]:
                # literals
                return match_pattern_r(pattern[1:], expr[1:], bindings)
            elif pattern[0][0] == TokenType.WORD:
                if pattern[0][1] in bindings:
                    raise SyntaxError(
                        f"Duplicate binding: {pattern[0][1]}")
                bindings[pattern[0][1]] = expr[0]
                return match_pattern_r(pattern[1:], expr[1:], bindings)
            else:
                return None
        return match_pattern_r(pattern, expr, {})

    def apply_template(template: List, bindings: Dict[str, Any]) -> List[Any]:
        def apply_template_r(temp: List):
            if not temp:
                return []
            if isinstance(temp[0], List):
                return [apply_template_r(t) for t in temp]
            if temp[0][0] == TokenType.WORD:
                if temp[0][1] in bindings:
                    return [bindings[temp[0][1]]] + apply_template_r(temp[1:])
                if (temp[0], temp[1]) in bindings:
                    return bindings[(temp[0], temp[1])]  # 无需列表包裹
                else:
                    return [temp[0]] + apply_template_r(temp[1:])
            else:
                return [temp[0]] + apply_template_r(temp[1:])
        return apply_template_r(template)

    def transformer(expr, env):
        for pattern, template in zip(patterns, templates):
            result = match_pattern(pattern, expr)
            if result is not None:
                if isinstance(result, Tuple) and result[0] == "binding":
                    bindings = result[1]
                    return apply_template(template, bindings)
                elif isinstance(result, Tuple) and result[0] == "success":
                    return template
                else:
                    raise SyntaxError("Invalid pattern template")
        raise SyntaxError(
            "No matching pattern found for expression: \n" + str(expr) + "\n in:\n\n" + functools.reduce(lambda x, y: x + "\n" + y, map(str, patterns)))

    return transformer


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
}


def unbox_list(lst):
    return [x[1] for x in lst]


def unbox_list_rec(lst):
    return [unbox_list_rec(x) if isinstance(x, list) else x[1] for x in lst]
# 定义解释器的函数


class FakeList():
    def __init__(self, eles):
        self.eles = eles


def quasiquote(expr, env):
    def add_type(ep):
        match ep:
            case list():
                return [add_type(x) for x in ep]
            case str():
                return (TokenType.STRING, ep)
            case int() | float():
                return (TokenType.NUMBER, ep)
            case (_, _):
                return ep
            case _:
                raise SyntaxError(f"Invalid syntax: {ep}")

    def eval_sexp_keep_type(ep):
        res = eval_sexpression(ep, env)
        return res
        match res:
            case list():
                return [add_type(x) for x in res]
            case _:
                return add_type(res)

    def handle_ele(ep):
        def reducer(acc, x):
            res = handle_ele(x)
            if isinstance(res, FakeList):
                return acc + res.eles
            else:
                acc.append(res)
                return acc
        match ep:
            case [(TokenType.WORD, "unquote"), x]:
                return eval_sexp_keep_type(x)
            case [(TokenType.WORD, "unquote-splicing"), x]:
                return FakeList(eval_sexp_keep_type(x))
            case list():
                return functools.reduce(reducer, ep, [])
            case (kind, val):
                return ep[1]
            case _:
                return ep[1]

    return handle_ele(expr)


def eval_sexpression(expr: List | Token, env: Dict):
    # expr = macro_expand(expr, env)  # Macro expansion
    if isinstance(expr, Tuple):
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
    # process pair
    match expr:
        case [va, (TokenType.CONS, _), vb]:
            return [va, vb]

    match [expr[0][1], *expr[1:]]:
        case ["cons", va, vb]:
            return [eval_sexpression(va, env), eval_sexpression(vb, env)]
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
            if isinstance(exp, list):
                return unbox_list_rec(exp)
            return exp[1]
        case ["quasiquote", exp]:
            return quasiquote(exp, env)
        case ["if", test, conseq]:
            if eval_sexpression(test, env):
                return eval_sexpression(conseq, env)
            return None
        case ["if", test, conseq, alt]:
            exp = conseq if eval_sexpression(test, env) else alt
            return eval_sexpression(exp, env)
        case ["define", [(TokenType.WORD, func_name), *params], body]:
            env[func_name] = lambda *args: eval_sexpression(
                body, env | dict(zip(unbox_list(params), args)))
        case ["define", symbol, exp]:
            env[symbol[1]] = eval_sexpression(exp, env)
        case ["lambda", params, body]:
            return lambda *args: eval_sexpression(body, env | dict(zip(unbox_list(params), args)))
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
        case ["define-syntax", (TokenType.WORD, macro_name), body]:
            define_syntax(expr, env)
        case ["syntax-rules", [*literals], *pattern]:
            return syntax_rules(literals, pattern)
        case [proc, *args]:
            proc = eval_sexpression(expr[0], env)
            match proc:
                case ("macro", transformer):
                    result_code = transformer(expr, env)
                    return eval_sexpression(result_code, env)
            args = [eval_sexpression(arg, env) for arg in args]
            return proc(*args)
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
        return run(code, env)


current_dir = os.path.dirname(os.path.abspath(__file__))
std_file_path = os.path.join(current_dir, 'std.scm')
run_file(std_file_path, global_env)


def run_file_yield(filename, env) -> Generator:
    with open(filename, "r", encoding="utf-8") as file:
        code = file.read()
        yield from run_yield(
            code, env)


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
