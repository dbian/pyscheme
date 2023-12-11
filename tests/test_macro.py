import unittest
import pyscheme


class TestParser(unittest.TestCase):

    def test_macro(self):
        env = pyscheme.new_env()
        f = pyscheme.run_file("tests/macro.scm", env)
        print(f)


if __name__ == '__main__':
    unittest.main()
