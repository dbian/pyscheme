(define-syntax unquote-splicing
  (syntax-rules ()
    ((_ lst)
     lst)))

(define-syntax unquote
  (syntax-rules ()
    ((_ x)
     x)))

(define-syntax quasiquote
  (syntax-rules (unquote unquote-splicing)
    ((_ (unquote-splicing (unquote x)) rest ...)
     (append x (quasiquote rest ...)))
    ((_ (unquote x) rest ...)
     (cons x (quasiquote rest ...)))
    ((_ (quasiquote x) rest ...)
     (quasiquote (quasiquote x) rest ...))
    ((_ (x . y) rest ...)
     (cons (quasiquote x) (quasiquote y) rest ...))
    ((_ x rest ...)
     (cons (quasiquote x) (quasiquote rest ...)))))
  
