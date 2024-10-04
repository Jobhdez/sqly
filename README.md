# sqly
A SQL dsl in the making ...

### SELECT
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

```
### INSERT 
```
sql.rkt> (insert into books (author book) (values (herman sicp)))
'("INSERT into books(author,book) values (~a,~a)" ("herman" "sicp
```

### UPDATE
```
sql.rkt> (update books (set ((= date 1) (= publisher 2))))
'("UPDATE books SET date = ~a, publisher = ~a" (1 2))
sql.rkt> (update books (set ((= date 1) (= f 3))) (where (= id 1)))
'("books SET date = ~a, f = ~a WHERE id=~a" (1 3 "1"))
```

### DELETE
```
sql.rkt> (delete from books)
'("DELETE FROM books")
sql.rkt> (delete (from books) (where (= id 1)))
'("DELETE FROM books WHERE id=~a" ("1"))
sql.rkt> 
```
