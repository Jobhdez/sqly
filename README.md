# sqly
A SQL dsl in the making ...

```racket 
sql.rkt> (select (book author) (from books) (where (and (> id 3) (< id 10))))
'("SELECT book,author FROM books WHERE id > ~a  AND id < ~a " ("3" "10"))
————— run sql.rkt —————
sql.rkt> (select (book author) (from books))
'("SELECT book,author FROM books")
sql.rkt> (select (book author) (from books) (limit 10))
'("SELECT book,author FROM books LIMIT ~a" ("10"))
sql.rkt> (select (book author) (from books) (order-by author ASC))
'("SELECT book,author FROM books ORDER BY ~a " ("ASC" "author"))
sql.rkt> (select (book author) (from books) (where (= id 2)))
'("SELECT book,author FROM books WHERE id=~a" ("2"))
sql.rkt> (select (book author) (from books) (left-join books2 on (= books-id books2-id)))
'("SELECT book,author FROM books LEFT JOIN books2 ON ~a  =~a" ("books-id" "books2-id"))
sql.rkt> (select (book author) (where (id in (2 3 4))))
match: no matching clause for '((book author) (where (id in (2 3 4))))
 sql.rkt:24:2
sql.rkt> (select (book author) (from books) (where (id in (2 3 4))))
symbol->string: contract violation
  expected: symbol?
  given: '(2 3 4)
Context (plain):
 sql.rkt:91:0 where->string
sql.rkt> (select (book author) (from books) (where (books (in 3 4 5))))
'("SELECT book,author FROM books WHERE books in (?,?,?)" ("3" "4" "5"))
————— run sql.rkt —————
sql.rkt> (select (book author) (from books) (where (books (in 3 4 5))))
'("SELECT book,author FROM books WHERE books in (~a,~a,~a)" ("3" "4" "5"))
sql.rkt> 
```

```racket
sql.rkt> (select (film-id title) (from film) (where (film-id (in 1 2 3))))
"SELECT film-id,title FROM film WHERE film-id in (1,2,3)"
sql.rkt> (select (film-id title) (from film) (where (film-id (in 1 2 3))) (order-by title ASC))
"SELECT film-id,title FROM film WHERE film-id in (1,2,3)  ORDER BY title ASC"
————— run sql.rkt —————
————— run sql.rkt —————
sql.rkt> (update (books (set ((= date 1)(= publisher 3)))))
"UPDATE 1,3"
————— run sql.rkt —————
 sql.rkt:136:26
————— run sql.rkt —————
sql.rkt> (update (books (set ((= date 1)(= publisher 3)))))
"UPDATE books SET date = 1,publisher = 3"
————— run sql.rkt —————
sql.rkt> (update (books) (set ((= date 1)(= publisher 3))))
————— run sql.rkt —————
sql.rkt> (update (books (set ((= date 1)(= publisher 3)))))
"UPDATE books SET date = 1,publisher = 3"
————— run sql.rkt —————
sql.rkt> (update books (set ((= date 1)(= publisher 3))))
"UPDATE books SET date = 1,publisher = 3"
sql.rkt> (update books (set ((= date 2))))
"UPDATE books SET date = 2"
————— run sql.rkt —————
sql.rkt> (update books (set ((= date 1) (= publisher 3))) (where (and (> id 2) (< id 10))))
"UPDATE books SET date = 1,publisher = 3 WHERE id>2 AND id<10"
sql.rkt> 
```

