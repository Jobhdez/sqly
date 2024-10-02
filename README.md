# sqly
A SQL dsl in the making ...

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

