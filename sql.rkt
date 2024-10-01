#lang racket

(define-syntax-rule (select exprs ...)
  (string-append
   "SELECT " (compile-select  'exprs ...)))

(define (compile-select . expr)
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

(define-syntax-rule (insert exprs ...)
  (string-append
   "INSERT " (compile-insert 'exprs ...)))

(define (compile-insert . exp)
  (match exp
    [(list 'into table-name (list fields ...) (list 'values (list fields-vals ...)))
     (let [(joined-fields (join-fields fields))
           (joined-val-fields (join-fields fields-vals))]
       (string-append
        "into "
        (symbol->string table-name)
        "("
        joined-fields
        ") "
        "values "
        "("
        joined-val-fields
        ")"))]))
      

(define (join-fields fields)
  (define (field->string field)
    (if (number? field)
        (number->string field)
        (symbol->string field)))
  
  (string-join (map field->string fields) ","))
