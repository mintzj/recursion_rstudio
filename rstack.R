library(rstackdeque)

rows <- rstack()
rows <- insert_top(rows, list(name = "Bob", age = 24))
rows <- insert_top(rows, list(name = "Joe", age = 27))

df <- as.data.frame(rows)
print(df)