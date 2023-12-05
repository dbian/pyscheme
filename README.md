Minimal r7rs scheme python library with a repl. Made by gpt, maintained by human.

# Warning

Under heavy development, use it at your own risk.

# Install

```shell
pip install pyscheme
```

## You may try latest features by using git to install

```shell
pip install git+https://github.com/dbian/pyscheme.git
```

# Implemented Features

- lexical scope `let`,`let*`
- `begin`, `if`, `lambda`, `define`... etc.
- REPL, with clojure like `*1` `*2` `*3` result cache support
- install_func to extend scheme language, you can use it to install global variables too.
- single line comment, start with `;`
- more

# Usage


```python
import pyscheme.scheme as sc
env = sc.new_env()
assert sc.run("(define bb (lambda (aa) (+ aa 2))) (bb 5)", env) == 7

def str_format_func(f, *args):
    return f % args
sc.install_func("format", str_format_func)

```

# Todo

- macros system
- quote `'` support
- module system
- more