
import unittest
import pyscheme.parser as parser
from pyscheme.tokenizer import TokenType


class TestParser(unittest.TestCase):
    def test_parse_number(self):
        r = parser.parse([(TokenType.NUMBER, 1)])
        self.assertEqual(r, (TokenType.NUMBER, 1))

    def test_parse_list(self):
        r = parser.parse(
            [(TokenType.PAREN_L, "("), (TokenType.NUMBER, 2), (TokenType.PAREN_R, ")")])
        self.assertEqual(r, [(TokenType.NUMBER, 2)])

    def test_parse_string(self):
        r = parser.parse([(TokenType.STRING, "a")])
        self.assertEqual(r, (TokenType.STRING, "a"))

    def test_parse_word(self):
        r = parser.parse([(TokenType.WORD, "a")])
        self.assertEqual(r, (TokenType.WORD, "a"))

    def test_parse_quote(self):
        r = parser.parse([(TokenType.QUOTE, "'"), (TokenType.WORD, "a")])
        self.assertEqual(r, [(TokenType.WORD, "quote"), (TokenType.WORD, "a")])

    def test_parse_double_quote(self):
        r = parser.parse(
            [(TokenType.QUOTE, "'"), (TokenType.QUOTE, "'"), (TokenType.WORD, "a")])
        self.assertEqual(r, [(TokenType.WORD, "quote"), [
                         (TokenType.WORD, "quote"), (TokenType.WORD, "a")]])

    def test_parse_quote_list(self):
        r = parser.parse([(TokenType.QUOTE, "'"), (TokenType.PAREN_L, "("),
                          (TokenType.WORD, "a"), (TokenType.PAREN_R, ")")])
        self.assertEqual(r, [(TokenType.WORD, "quote"),
                             [(TokenType.WORD, "a")]])

    def test_parse_qquote(self):
        r = parser.parse([(TokenType.QQUOTE, "`"), (TokenType.WORD, "a")])
        self.assertEqual(
            r, [(TokenType.WORD, "quasiquote"), (TokenType.WORD, "a")])

    def test_parse_qquote_list(self):
        r = parser.parse([(TokenType.QUOTE, "`"), (TokenType.PAREN_L, "("),
                          (TokenType.WORD, "a"), (TokenType.PAREN_R, ")")])
        self.assertEqual(r, [(TokenType.WORD, "quasiquote"),
                             [(TokenType.WORD, "a")]])

    def test_parse_comma(self):
        r = parser.parse([(TokenType.QUOTE, "`"), (TokenType.PAREN_L, "("), (TokenType.COMMA, ","),
                          (TokenType.WORD, "a"), (TokenType.PAREN_R, ")")])
        self.assertEqual(r, [(TokenType.WORD, "quasiquote"),
                             [[(TokenType.WORD, "unquote"), (TokenType.WORD, "a")]]])

    def test_parse_comma_list(self):
        r = parser.parse([(TokenType.QUOTE, "`"), (TokenType.PAREN_L, "("), (TokenType.COMMA_AT, ",@"),
                          (TokenType.WORD, "a"), (TokenType.PAREN_R, ")")])
        self.assertEqual(r, [(TokenType.WORD, "quasiquote"),
                             [[(TokenType.WORD, "unquote-splicing"), (TokenType.WORD, "a")]]])

    def test_cons(self):
        r = parser.parse(
            [(TokenType.PAREN_L, "("), (TokenType.WORD, "a"), (TokenType.CONS, " . "), (TokenType.WORD, "b"), (TokenType.PAREN_R, ")")])
        self.assertEqual(
            r, [(TokenType.WORD, "a"), (TokenType.CONS, " . "), (TokenType.WORD, "b")])
        # r = parser.parse(
        #     [(TokenType.PAREN_L, "("), (TokenType.PAREN_R, ")")])
        # self.assertEqual(r, [])


if __name__ == '__main__':
    unittest.main()
