library(rstackdeque)

## Basic Rstack insertions
s <- rstack()
s <- insert_top(s, "A")
s <- insert_top(s, "B")
s <- insert_top(s, "C")
print(s)

## peeking
print(peek_top(s))

## removal
s <- without_top(s)
print(s)

## rstacks can hold "rows" (either as lists or 1-row DFs) to be later converted into a data frame
## but each row has to have the same names in the same order
rows <- rstack()
rows <- insert_top(rows, list(name = "Bob", age = 24))
rows <- insert_top(rows, list(name = "Joe", age = 27))

df <- as.data.frame(rows)
print(df)