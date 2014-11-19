library(stringr)
library(ggplot2)
library(rstackdeque)

fib <- function(n) {
  CALL_COUNTER <<- CALL_COUNTER + 1
  thiscall <- str_c("fib:n=", as.character(n))
  CALL_STACK <<- insert_top(CALL_STACK, thiscall)
  #print_string_stack(CALL_STACK)
  if(n == 1) {
    CALL_STACK <<- without_top(CALL_STACK)
    #print_string_stack(CALL_STACK)
    return(1)
  } else if(n == 2) {
    CALL_STACK <<- without_top(CALL_STACK)
    #print_string_stack(CALL_STACK)
    return(1)
  } else {
    a <- fib(n-1)
    b <- fib(n-2)
    c <- a + b
    CALL_STACK <<- without_top(CALL_STACK)
    #print_string_stack(CALL_STACK)
    return(c)
  }
}

print_string_stack <- function(s) {
  char_vec <- as.character(as.list(s))
  print(rev(char_vec))
}

CALL_STACK <<- rstack()
info <- rstack() # a simple thing to storse rows in

for(i in seq(1,15)) {
  CALL_COUNTER <<- 0
  inforow <- list(n = i, fibn = fib(i), callsn = CALL_COUNTER)
  info <- insert_top(info, inforow)
}

infodf <- as.data.frame(info)

p <- ggplot(infodf) +
  geom_line(aes(x = n, y = fibn), color = "green") +
  geom_line(aes(x = n, y = callsn), color = "red")
plot(p)