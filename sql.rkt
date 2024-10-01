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
    [(list (list fields ...) (list 'from table) (list 'limit val))
     (string-append
      (join-fields fields)
      " FROM "
      (symbol->string table)
      " LIMIT "
      (field->string val))]
    [(list (list fields ...) (list 'from table) (list 'where (list where-exprs ...)))
     (let [(joined-fields (join-fields fields))
           (whereeq (where->string where-exprs))]
       (string-append (join-fields fields)
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
  (string-join (map field->string fields) ","))

(define (where->string . exprs)
  (match exprs
    [(list (list '= id val))
     (string-append
      (field->string id)
      " = "
      (field->string val))]
    [(list (list 'and (list '= id val) (list '= id2 val2)))
     (string-append
      (field->string id)
      " = "
      (field->string val)
      " AND "
      (field->string id2)
      " = "
      (field->string val2))]))

  (define (field->string field)
    (if (number? field)
        (number->string field)
        (symbol->string field)))
