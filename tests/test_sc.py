import unittest
import sys

sys.path.append("..")
print(sys.path)
from pyscheme import scheme


class TestT(unittest.TestCase):
    def test_t(self):
        env = scheme.new_env()
        self.assertEqual(scheme.run("(+ 1 34)", env), 35)
        self.assertEqual(scheme.run("(define aa 2) (+ aa 2)", env), 4)
        self.assertEqual(
            scheme.run("(define bb (lambda (aa) (+ aa 2))) (bb 2)", env), 4
        )
        scheme.run("(define aa 2) (display aa)", env)
        self.assertEqual(scheme.run("(map bb (list 4 5 6))", env), [6, 7, 8])
        self.assertEqual(
            scheme.run('(map (lambda (x) x) (list "a" "bb" "c"))', env),
            ["a", "bb", "c"],
        )
        self.assertEqual(
            scheme.run('(map (lambda (x) x) (list "abc你好世界"))', env),
            ["abc你好世界"],
        )

    def test_list(self):
        env = scheme.new_env()
        self.assertEqual(scheme.run("(list 1 2 3)", env), [1, 2, 3])
        self.assertEqual(scheme.run('(list "abc你好世界")', env), ["abc你好世界"])


unittest.main()
