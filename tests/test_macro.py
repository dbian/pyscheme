import unittest
import pyscheme


class TestParser(unittest.TestCase):

    def test_macro(self):
        env = pyscheme.new_env()
        f = pyscheme.run_file("tests/macro.scm", env)
        self.assertEqual(f, 1)

    def test_macro2(self):
        env = pyscheme.new_env()
        f = pyscheme.run_file("tests/macro2.scm", env)
        self.assertEqual(f, 1)

    def test_macro3(self):
        env = pyscheme.new_env()
        f = pyscheme.run_file("tests/macro_rec.scm", env)
        self.assertEqual(f, 1)


if __name__ == '__main__':
    unittest.main()
