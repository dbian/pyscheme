import time
from typing import Dict
import pyscheme
import unittest


class TestEval(unittest.TestCase):
    def test_t(self):
        env = pyscheme.new_env()
        self.assertEqual(pyscheme.run("(+ 1 34)", env), 35)
        self.assertEqual(pyscheme.run("(define aa 2) (+ aa 2)", env), 4)
        self.assertEqual(
            pyscheme.run("(define bb (lambda (aa) (+ aa 2))) (bb 2)", env), 4
        )
        self.assertEqual(pyscheme.run(
            "(map bb (list 4 5 6))", env), [6, 7, 8])
        self.assertEqual(
            pyscheme.run('(map (lambda (x) x) (list "a" "bb" "c"))', env),
            ["a", "bb", "c"],
        )
        self.assertEqual(
            pyscheme.run('(map (lambda (x) x) (list "abc你 ; 好世界"))', env),
            ["abc你 ; 好世界"],
        )

    def test_list(self):
        env = pyscheme.new_env()
        self.assertEqual(pyscheme.run("(list 1 2 3)", env), [1, 2, 3])
        self.assertEqual(pyscheme.run('(list "abc你好 世界")', env), ["abc你好 世界"])

    def test_let(self):
        env = pyscheme.new_env()
        self.assertEqual(pyscheme.run("(let ((a 1) (b 2)) (+ a b))", env), 3)
        self.assertRaises(NameError, lambda: pyscheme.run(
            "(let ((a 1) (b (+ 1 a))) (+ a b))", env))

    def test_let_star(self):
        env = pyscheme.new_env()
        self.assertEqual(pyscheme.run("(let* ((a 1) (b 2)) (+ a b))", env), 3)
        self.assertEqual(pyscheme.run(
            "(let* ((a 1) (b (+ 1 a))) (+ a b))", env), 3)

    def test_let_lexical_scope(self):
        env = pyscheme.new_env()
        self.assertEqual(pyscheme.run(
            "(let ((a 1)) (+ a (let ((a 2)) a)))", env), 3)

    def test_simple_comment(self):
        env = pyscheme.new_env()
        self.assertEqual(pyscheme.run("(+ 1 314) ; wdq \"dq", env), 315)
        self.assertEqual(pyscheme.run(
            "(+ 1 314) ; wdq \"dq\n; aaa", env), 315)

    def test_define_func(self):
        env = pyscheme.new_env()
        self.assertEqual(
            pyscheme.run("(define (bb aa) (+ aa 2)) (bb 2)", env), 4
        )


def yd(x):
    yield x


class TestYield(unittest.TestCase):
    def test_yield(self):
        env = pyscheme.new_env()
        pyscheme.put("yd", yd, env)
        pyscheme.put("sleep", lambda x: time.sleep(x), env)
        f = pyscheme.run_file_yield("tests/y1.scm", env)
        v = next(f)
        self.assertEqual(v, 1)
        print(v)
        self.assertEqual(next(f), None)
        self.assertEqual(next(f), 4)
        self.assertEqual(next(f), 2)

    def test_loadfile(self):
        env = pyscheme.new_env()
        pyscheme.put("yd", yd, env)
        pyscheme.put("sleep", lambda x: time.sleep(x), env)
        pyscheme.put("load", lambda x: pyscheme.run_file_yield(x, env), env)
        f = pyscheme.run_yield("(load \"tests/y1.scm\")", env)
        v = next(f)
        self.assertEqual(v, 1)
        print(v)
        self.assertEqual(next(f), None)
        self.assertEqual(next(f), 4)
        self.assertEqual(next(f), 2)


if __name__ == '__main__':
    unittest.main()
