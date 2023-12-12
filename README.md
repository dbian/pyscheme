Minimal r7rs scheme python library with a repl. Made by gpt, maintained by human.

# Install

```shell
pip install pyscheme
```

## Using git to install latest version

```shell
pip install git+https://github.com/dbian/pyscheme.git
```

# Implemented Features

- Lexical scope `let`,`let*`. `begin`, `if`, `lambda`, `define`... etc.
- Full featured Macro system, which means you can use `_` and `...` in pattern matching rules.
- REPL, with clojure like `*1` `*2` `*3` result cache support
- Use `put` to extend scheme language easily with power python ecosystem.
- Single line comment, start with `;`
- More to discover...

# Usage


```python
import pyscheme
env = pyscheme.new_env()
assert pyscheme.run("(define bb (lambda (aa) (+ aa 2))) (bb 5)", env) == 7

def str_format_func(f, *args):
    return f % args
pyscheme.put("format", str_format_func)
pyscheme.put("variable", 123)

```

# Future plans

- [ ] easy template interpolation support aka.: "`", "," and ",@"
- [ ] macros system add hygienic variable support
- [ ] more standard functions
- module system
- more
