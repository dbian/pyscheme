import unittest
import pyscheme.tokenizer as tokenizer


class TestT(unittest.TestCase):
    def test_comment(self):
        scheme_code = '''
        ; ( This )is a comment
        (define x 10)
        (define y 20.5)
        (define str "Hello#|,() world!|# ; fake comment")
        (define 'a 30)
        `(,display ,@(list a b))
    '''
        tk = tokenizer.tokenize(scheme_code)
        self.assertEqual(tk[0], (tokenizer.TokenType.PAREN_L, "("))
        self.assertEqual(tk[8], (tokenizer.TokenType.NUMBER, 20.5))
        self.assertEqual(
            tk[13], (tokenizer.TokenType.STRING, "Hello#|,() world!|# ; fake comment"))
        self.assertEqual(tk[14], (tokenizer.TokenType.PAREN_R, ")"))
        self.assertEqual(tk[17], (tokenizer.TokenType.QUOTE, "'"))
        self.assertEqual(tk[21], (tokenizer.TokenType.QQUOTE, "`"))
        self.assertEqual(tk[23], (tokenizer.TokenType.COMMA, ","))
        self.assertEqual(tk[25], (tokenizer.TokenType.COMMA_AT, ",@"))

        scheme_code = '''; This is a comment'''
        tk = tokenizer.tokenize(scheme_code)
        self.assertEqual(len(tk), 0)

    def test_quote(self):
        scheme_code = '''
            (define a 30)
        '''
        tk = tokenizer.tokenize(scheme_code)
        self.assertEqual(tk[2], (tokenizer.TokenType.WORD, "a"))
        self.assertEqual(tk[3], (tokenizer.TokenType.NUMBER, 30))
        self.assertEqual(tk[4], (tokenizer.TokenType.PAREN_R, ")"))

    @unittest.skip("not implemented yet")
    def test_wrong(self):
        scheme_code = '"aa'
        tk = tokenizer.tokenize(scheme_code)
        print(tk)
        self.assertEqual(len(tk), 0)

    def test_oper(self):
        scheme_code = '+'
        tk = tokenizer.tokenize(scheme_code)
        self.assertEqual(tk[0], (tokenizer.TokenType.WORD, "+"))

    def test_double_quote(self):
        scheme_code = "''a"
        tk = tokenizer.tokenize(scheme_code)
        self.assertEqual(tk[0], (tokenizer.TokenType.QUOTE, "'"))
        self.assertEqual(tk[1], (tokenizer.TokenType.QUOTE, "'"))
        self.assertEqual(tk[2], (tokenizer.TokenType.WORD, "a"))


if __name__ == '__main__':
    unittest.main()
