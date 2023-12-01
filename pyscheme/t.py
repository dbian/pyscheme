import unittest
import pyscheme.scheme as scheme


class TestT(unittest.TestCase):
    def test_t(self):
        self.assertEqual(scheme.run("(+ 1 34)"), 35)
        self.assertEqual(scheme.run("(define aa 2) (+ aa 2)"), 4)
        scheme.run("(define aa 2) (display aa)")


unittest.main()
