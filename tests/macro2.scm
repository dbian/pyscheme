(define-syntax when  ; 定义宏 `when`
  (syntax-rules ()   ; 定义宏的模式
    ((_ condition body ...)  ; 宏的模式匹配
     (if condition
         (begin body ...)))))  ; 生成展开后的代码

;; 示例用法
(define x 5)
(when (> x 0)  ; 当 x 大于 0 时执行
  (display x)
  1)