import time
from typing import Dict
from pyscheme import evaluator
import unittest


class TestEval(unittest.TestCase):
    def test_t(self):
        env = evaluator.new_env()
        self.assertEqual(evaluator.run("(+ 1 34)", env), 35)
        self.assertEqual(evaluator.run("(define aa 2) (+ aa 2)", env), 4)
        self.assertEqual(
            evaluator.run("(define bb (lambda (aa) (+ aa 2))) (bb 2)", env), 4
        )
        self.assertEqual(evaluator.run(
            "(map bb (list 4 5 6))", env), [6, 7, 8])
        self.assertEqual(
            evaluator.run('(map (lambda (x) x) (list "a" "bb" "c"))', env),
            ["a", "bb", "c"],
        )
        self.assertEqual(
            evaluator.run('(map (lambda (x) x) (list "abc你 ; 好世界"))', env),
            ["abc你 ; 好世界"],
        )

    def test_list(self):
        env = evaluator.new_env()
        self.assertEqual(evaluator.run("(list 1 2 3)", env), [1, 2, 3])
        self.assertEqual(evaluator.run('(list "abc你好 世界")', env), ["abc你好 世界"])

    def test_let(self):
        env = evaluator.new_env()
        self.assertEqual(evaluator.run("(let ((a 1) (b 2)) (+ a b))", env), 3)
        self.assertRaises(NameError, lambda: evaluator.run(
            "(let ((a 1) (b (+ 1 a))) (+ a b))", env))

    def test_let_star(self):
        env = evaluator.new_env()
        self.assertEqual(evaluator.run("(let* ((a 1) (b 2)) (+ a b))", env), 3)
        self.assertEqual(evaluator.run(
            "(let* ((a 1) (b (+ 1 a))) (+ a b))", env), 3)

    def test_let_lexical_scope(self):
        env = evaluator.new_env()
        self.assertEqual(evaluator.run(
            "(let ((a 1)) (+ a (let ((a 2)) a)))", env), 3)

    def test_simple_comment(self):
        env = evaluator.new_env()
        self.assertEqual(evaluator.run("(+ 1 314) ; wdq \"dq", env), 315)
        self.assertEqual(evaluator.run(
            "(+ 1 314) ; wdq \"dq\n; aaa", env), 315)

    def test_define_func(self):
        env = evaluator.new_env()
        self.assertEqual(
            evaluator.run("(define (bb aa) (+ aa 2)) (bb 2)", env), 4
        )


def yd(x):
    yield x


class TestYield(unittest.TestCase):
    def test_yield(self):
        env = evaluator.new_env()
        evaluator.put("yd", yd, env)
        evaluator.put("sleep", lambda x: time.sleep(x), env)
        f = evaluator.run_file_yield("tests/y1.scm", env)
        v = next(f)
        self.assertEqual(v, 1)
        print(v)
        self.assertEqual(next(f), None)
        self.assertEqual(next(f), 4)
        self.assertEqual(next(f), 2)

    def test_loadfile(self):
        env = evaluator.new_env()
        evaluator.put("yd", yd, env)
        evaluator.put("sleep", lambda x: time.sleep(x), env)
        evaluator.put("load", lambda x: evaluator.run_file_yield(x, env), env)
        f = evaluator.run_yield("(load \"tests/y1.scm\")", env)
        v = next(f)
        self.assertEqual(v, 1)
        print(v)
        self.assertEqual(next(f), None)
        self.assertEqual(next(f), 4)
        self.assertEqual(next(f), 2)


if __name__ == '__main__':
    unittest.main()
