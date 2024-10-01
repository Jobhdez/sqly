#lang racket

(define-syntax-rule (select exprs ...)
  (string-append
   "SELECT " (select-expr  'exprs ...)))

(define (select-expr . expr)
  (match expr
    [(list (list fields ...) (list 'from table))
     (string-append
      (string-join (map symbol->string fields) ",")
      " FROM "
      (symbol->string table))]
    [(list (list fields ...) (list 'from table) (list 'where (list '= id val)))
     (let [(whereeq (string-append (symbol->string id) " = " (number->string val)))]
       (string-append
        (string-join (map symbol->string fields) ",")
        " FROM "
        (symbol->string table)
        " WHERE "
        whereeq))]))
      
