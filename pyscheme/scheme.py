import operator
import math
from typing import List


def list_func(*x):
    return list(x)


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
    "list": list_func,
    "list-tail": lambda lst, k: lst[k:] if isinstance(lst, list) else None,
    "map": lambda x, y: list(map(x, y)),
}


# 定义解释器的函数
def eval_sexpression(expr, env):
    match expr:
        case expr if isinstance(expr, str):
            if expr in env:
                return env[expr]
            if expr.startswith('"') and expr.endswith('"'):
                return expr[1:-1]
            raise NameError("Undefined symbol: " + expr)

        case expr if not isinstance(expr, list):
            return expr
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

        case ["define", symbol, exp]:
            env[symbol] = eval_sexpression(exp, env)

        case ["lambda", params, body]:
            return lambda *args: eval_sexpression(body, env | dict(zip(params, args)))
        case ["begin", *exps]:
            for exp in exps:
                result = eval_sexpression(exp, env)
            return result
        case ["let", bindings, *body]:
            new_env = env.copy()
            for binding in bindings:
                symbol, exp = binding
                value = eval_sexpression(exp, env)
                new_env[symbol] = value
            return eval_sexpression(["begin", *body], new_env)
        case [proc, *args]:
            proc = eval_sexpression(proc, env)
            args = [eval_sexpression(arg, env) for arg in args]
            return proc(*args)
        case _:
            raise SyntaxError("Invalid syntax")


# 安装函数到解释器环境
def install_func(name, func):
    global global_env
    global_env[name] = func


# 运行字符串源码
def run(code: str, env: dict):
    lines = code.split("\n")
    tokens = []
    for line in lines:
        # 移除行注释
        line = line.split(";")[0]
        tokens.extend(line.replace("(", " ( ").replace(")", " ) ").split())

    while len(tokens) > 0:
        exprs = parse(tokens)
        res = eval_sexpression(exprs, env)
    return res


# 运行文件
def run_file(filename, env):
    with open(filename, "r", encoding="utf-8") as file:
        code = file.read()
        run(code, env)


# 解析
def parse(tokens: List[str]):
    if len(tokens) == 0:
        raise SyntaxError("Unexpected EOF")

    token = tokens.pop(0)
    if token == "(":
        expr = []
        while tokens[0] != ")":
            expr.append(parse(tokens))
        tokens.pop(0)  # 弹出')'
        return expr
    elif token == ")":
        raise SyntaxError("Unexpected )")
    else:
        return atomize(token)


# 原子化
def atomize(token):
    try:
        return int(token)
    except ValueError:
        try:
            return float(token)
        except ValueError:
            return token


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


install_func("display", display)

# python bridge
install_func("attr", getattr)

if __name__ == "__main__":
    # 启动交互式命令行界面
    repl()
