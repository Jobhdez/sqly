#lang racket

;;;; == macros ==

(define-syntax-rule (select exprs ...)
  (string-append
   "SELECT " (compile-select  'exprs ...)))

(define-syntax-rule (insert exprs ...)
  (string-append
   "INSERT " (compile-insert 'exprs ...)))

(define-syntax-rule (update exprs ...)
  (string-append
   "UPDATE " (compile-update 'exprs ...)))

(define-syntax-rule (delete exprs ...)
  (string-append
   "DELETE " (compile-delete 'exprs ...)))

;;;; == compilers ==

;;; SELECT
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
    
    [(list (list fields ...) (list 'from table) (list 'order-by exp order))
     (string-append
      (join-fields fields)
      " FROM "
      (symbol->string table)
      " ORDER BY "
      (field->string exp)
      " "
      (field->string order))]
    
    [(list (list fields ...) (list 'from table) (list 'where (list where-exprs ...)))
     (let [(joined-fields (join-fields fields))
           (whereeq (where->string where-exprs))]
       (string-append (join-fields fields)
                      " FROM "
                      (symbol->string table)
                      " WHERE "
                      whereeq))]
    
    [(list (list fields ...) (list 'from table) (list 'where (list where-exprs ...)) (list 'order-by exp order))
     (string-append
      (join-fields fields)
      " FROM "
      (field->string table)
      " WHERE "
      (where->string where-exprs)
      " "
      " ORDER BY "
      (field->string exp)
      " "
      (field->string order))]

    [(list (list fields ...) (list 'from table) (list 'left-join table2 'on (list (? symbol? op) (? symbol? e) (? symbol? e2))))
     (string-append
      (join-fields fields)
      " FROM "
      (field->string table)
      " LEFT JOIN "
      (field->string table2)
      " ON "
      (field->string op)
      " "
      (field->string e)
      " "
      (field->string e2))]))

(define (where->string . exprs)
  (match exprs
    [(list (list (? symbol? op) (? symbol? id) val))
     (string-append
      (field->string id)
      (field->string op)
      (field->string val))]
    
    [(list (list (? and-or? oper) (list op id val) (list op2 id2 val2)))
     (string-append
      (field->string id)
      (field->string op)
      (field->string val)
      (if (eq? oper 'and) " AND " " OR ")
      (field->string id2)
      (field->string op2)
      (field->string val2))]

    [(list (list (? symbol? id) (list 'in fields ...)))
     (string-append
      (field->string id)
      " in "
      "("
      (join-fields fields)
      ")")]))
     

;;; INSERT
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

;;; UPDATE
(define (compile-update . exprs)
  (match exprs
    [(list (? symbol? val) (list 'set (list exps ...)))
     (match exps
       [(list (list '= id val2) ...)
        (string-append
         (field->string val)
        " SET "
        (join-equal-exps exps))])]

    [(list (? symbol? val) (list 'set (list exps ...)) (list 'where (list where-exps ...)))
     (match exps
       [(list (list '= id val2) ...)
        (string-append
         (field->string val)
         " SET "
         (join-equal-exps exps)
         " WHERE "
         (where->string where-exps))])]))

;;; DELETE
(define (compile-delete . exps)
  (match exps
    [(list 'from table)
     (string-append
      "FROM "
      (field->string table))]
    [(list (list 'from table) (list 'where (list where-exps ...)))
     (string-append
      "FROM "
      (field->string table)
      " WHERE "
      (where->string where-exps))]))

;;;; == Utils ==
(define (join-fields fields)
  (string-join (map field->string fields) ","))

(define (field->string field)
  (if (number? field)
      (number->string field)
      (symbol->string field)))

(define (and-or? e)
  (or (eq? e 'and)
      (eq? e 'or)))

(define (join-equal-exps exps)
  (define (exp->string exp)
    (match exp
      [(list '= id val)
       (string-append
        (field->string id)
       " = "
       (field->string val))]))
  (string-join (map exp->string exps) ","))
