# sqly
A SQL dsl in the making ...

### SELECT
```racket 
sqly.rkt> (select (book author) (from books) (where (and (> id 3) (< id 6))))
'("SELECT book,author FROM books WHERE id > $1  AND id < $2 " 3 6)

sqly.rkt> (select (book author) (from books))
'("SELECT book,author FROM books")

sqly.rkt> (select (book author) (from books) (limit 10))
'("SELECT book,author FROM books LIMIT $1" 10)

sqly.rkt> (select (book author) (from books) (order-by author ASC))
'("SELECT book,author FROM books ORDER BY $1 ASC" "author")

sqly.rkt>  (select (book author) (from books) (where (= id 2)))
'("SELECT book,author FROM books WHERE id=$1" 2)

sqly.rkt> (select (book author) (from books) (left-join books2 on (= books-id books2-id)))
'("SELECT book,author FROM books LEFT JOIN books2 ON $1  = $2" "books-id" "books2-id")

sqly.rkt> (select (book author) (from books) (where (books (in 3 4 5))))
'("SELECT book,author FROM books WHERE books in ($1,$2,$3)" 3 4 5)

```
### INSERT 
```racket
sqly.rkt> (insert into books (author book) (values (herman sicp)))
'("INSERT into books(author,book) values ($1,$2)" "herman" "sicp")
```

### UPDATE
```racket
sqly.rkt> (update books (set ((= date 1) (= publisher 2))))
'("UPDATE books SET date = $1, publisher = $2" 1 2)

sqly.rkt> (update books (set ((= date 1) (= f 3))) (where (= id 1)))
'("books SET date = $1, f = $2 WHERE id=$3" 1 3 1)

(update books (set ((= date 1) (= f 4))) (where (and (> id 3) (< id 5))))
'("books SET date = $1, f = $2 WHERE id >$3 AND id <$4" 1 4 3 5)
```

### DELETE
```racket
sqly.rkt> (delete from books)
'("DELETE FROM books")

sqly.rkt> (delete (from books) (where (= id 1)))
'("DELETE FROM books WHERE id=$1" 1)
```
