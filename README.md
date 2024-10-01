# sqly
A SQL dsl in the making ...

```racket
sql.rkt> (select (author pages genre) (from books) (where (= id 1)))
"SELECT author,pages,genre FROM books WHERE id = 1"
```
