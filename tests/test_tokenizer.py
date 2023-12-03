import unittest
import pyscheme.tokenizer as tokenizer


class TestT(unittest.TestCase):
    def test_comment(self):
        scheme_code = '''
        ; This is a comment
        #| This is a 
        ; block comment |#
        (define x 10)
        (define y 20.5)
        (define 'a 30)
        (define str "Hello#|, world!|# ; fake comment")
        (display (+ x y))
    '''
        tk = tokenizer.tokenize(scheme_code)
        self.assertEqual(tk[0], ("COMMENT_LINE", "; This is a comment\n"))

        scheme_code = '''; This is a comment'''
        tk = tokenizer.tokenize(scheme_code)
        self.assertEqual(tk[0], ("COMMENT_LINE", "; This is a comment"))


if __name__ == '__main__':
    unittest.main()
