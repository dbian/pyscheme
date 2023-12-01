import operator
import math
from typing import List

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
}


# 定义解释器的函数
def eval_sexpression(expr, env):
    # print("eval", expr)
    if isinstance(expr, str):  # 符号
        if expr in env:
            return env[expr]
        else:
            raise NameError("Undefined symbol: " + expr)
    elif not isinstance(expr, list):  # 常量
        return expr
    elif expr[0] == "quote":  # 引用
        (_, exp) = expr
        return exp
    elif expr[0] == "if":  # 条件表达式
        (_, test, conseq, alt) = expr
        exp = conseq if eval_sexpression(test, env) else alt
        return eval_sexpression(exp, env)
    elif expr[0] == "define":  # 定义函数或变量
        (_, symbol, exp) = expr
        env[symbol] = eval_sexpression(exp, env)
    elif expr[0] == "lambda":  # 定义匿名函数
        (_, params, body) = expr
        return lambda *args: eval_sexpression(body, dict(zip(params, args)))
    else:  # 过程调用
        proc = eval_sexpression(expr[0], env)
        args = [eval_sexpression(arg, env) for arg in expr[1:]]
        return proc(*args)


# 安装函数到解释器环境
def install_func(name, func):
    global global_env
    global_env[name] = func


# 运行字符串源码
def run(code: str):
    tokens = code.replace("(", " ( ").replace(")", " ) ").split()
    env_new = global_env.copy()
    while len(tokens) > 0:
        exprs = parse(tokens)
        eval_sexpression(exprs, env_new)


# 运行文件
def run_file(filename):
    with open(filename, "r") as file:
        code = file.read()
        run(code)


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


# 创建交互式命令行界面
def repl():
    while True:
        try:
            code = input("Scheme> ")
            run(code)
        except (EOFError, KeyboardInterrupt):
            print("\nGoodbye!")
            break
        except Exception as e:
            print("Error:", str(e))


# 安装自定义函数示例
def square(x):
    return x * x


def display(x):
    print(x)


install_func("square", square)
install_func("display", display)

if __name__ == "__main__":
    # 启动交互式命令行界面
    repl()
