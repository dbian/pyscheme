import re


def tokenize(scheme_code):
    # 定义正则表达式模式
    patterns = [
        # (r'#\|.*?\|#', 'COMMENT_BLOCK'),  # 匹配 /* 注释 */
        (r';.*\n?', 'COMMENT_LINE'),  # 匹配行注释
        (r'"(?:\\.|[^"])*"', 'STRING'),  # 匹配字符串
        (r'[-+]?\d+(?:\.\d+)?', 'NUMBER'),  # 匹配数字
        (r'[^\s\(\)"\';]+', 'WORD'),  # 匹配单词
        (r'\(|\)', 'PAREN'),  # 匹配括号
        (r"'", 'QUOTE'),  # 匹配引号
    ]

    token_regex = '|'.join('(?P<{}>{})'.format(name, pattern)
                           for pattern, name in patterns)
    tokens = []
    for match in re.finditer(token_regex, scheme_code):
        kind = match.lastgroup
        value = match.group()
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
