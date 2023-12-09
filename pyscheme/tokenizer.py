import re
from typing import List, Tuple
Token = Tuple[str, str | int | float]


class TokenType:
    COMMENT_BLOCK = "COMMENT_BLOCK"
    COMMENT_LINE = "COMMENT_LINE"
    STRING = "STRING"
    NUMBER = "NUMBER"
    WORD = "WORD"
    PAREN_L = "PAREN_L"
    PAREN_R = "PAREN_R"
    QUOTE = "QUOTE"
    QQUOTE = "QQUOTE"
    COMMA = "COMMA"
    COMMA_AT = "COMMA_AT"


def tokenize(scheme_code: str) -> List[Token]:
    # 定义正则表达式模式
    patterns = [
        # (r'#\|.*?\|#', TokenType.COMMENT_BLOCK),  # 匹配 /* 注释 */
        (r';.*\n?', TokenType.COMMENT_LINE),  # 匹配行注释
        (r'"(?:\\.|[^"])*"', TokenType.STRING),  # 匹配字符串
        (r'[-+]?\d+(?:\.\d+)?', TokenType.NUMBER),  # 匹配数字
        (r'\(', TokenType.PAREN_L),  # 匹配括号
        (r'\)', TokenType.PAREN_R),  # 匹配括号
        (r"'", TokenType.QUOTE),  # 匹配引号
        (r"`", TokenType.QQUOTE),
        (r",@", TokenType.COMMA_AT),
        (r",", TokenType.COMMA),
        (r'[^\s\(\)"\';]+', TokenType.WORD),  # 匹配单词
    ]

    token_regex = '|'.join('(?P<{}>{})'.format(name, pattern)
                           for pattern, name in patterns)
    tokens = []
    for match in re.finditer(token_regex, scheme_code):
        kind = match.lastgroup
        value = match.group()
        match kind:
            case TokenType.COMMENT_LINE:
                continue
            case TokenType.STRING:
                value = value[1:-1]
            case TokenType.NUMBER:
                try:
                    value = int(value)
                except ValueError:
                    value = float(value)
        tokens.append((kind, value))

    return tokens


if __name__ == '__main__':
    # 示例用法
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

    tokens = tokenize(scheme_code)
    for kind, value in tokens:
        print(kind, value)
