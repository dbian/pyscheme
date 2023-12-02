import sys

sys.path.append("..")
print(sys.path)
from pyscheme import scheme

env = scheme.new_env()

assert scheme.run("(define bb (lambda (aa) (+ aa 2))) (bb 5)", env) == 7
print(scheme.run("(list 1 2 3)", env))
assert scheme.run("(list 1 2 3)", env) == [1, 2, 3]
assert scheme.run('(list "abc你好世界")', env) == ["abc你好世界"]
