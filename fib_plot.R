library(stringr)
library(ggplot2)
library(rstackdeque)

fib <- function(n) {
  CALL_COUNTER <<- CALL_COUNTER + 1
  if(n == 1) {
    return(1)
  } else if(n == 2) {
    return(1)
  } else {
    a <- fib(n-1)
    b <- fib(n-2)
    c <- a + b
    return(c)
  }
}


info <- rstack() # a simple thing to store rows in

for(i in seq(1,15)) {
  CALL_COUNTER <<- 0
  inforow <- list(n = i, fibn = fib(i), callsn = CALL_COUNTER)
  info <- insert_top(info, inforow)
}

infodf <- as.data.frame(info)

p <- ggplot(infodf) +
  geom_line(aes(x = n, y = fibn)) +
  geom_line(aes(x = n, y = callsn))
plot(p)