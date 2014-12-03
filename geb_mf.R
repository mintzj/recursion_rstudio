library(ggplot2)
library(rstackdeque)
library(stringr)
library(hash)

CACHE <- hash()

m <- function(n) {
  thiscall <- str_c("m:",n)
  if(has.key(thiscall, CACHE)) {
    return(CACHE[[thiscall]])
  }
  if(n == 0) {
    return(0)
  } else {
    answer <- n - f(m(n-1))
    CACHE[[thiscall]] <- answer
    return(answer)
  }
}

f <- function(n) {
  thiscall <- str_c("f:",n)
  if(has.key(thiscall, CACHE)) {
    return(CACHE[[thiscall]])
  }
  if(n == 0) {
    return(1)
  } else {
    answer <- n - m(f(n-1))
    CACHE[[thiscall]] <- answer
    return(answer)
  }
}

info <- rstack()
for(i in seq(1,200)) {
  row <- list(n = i, mn = m(i), fn = f(i))
  info <- insert_top(info, row)
}

infodf <- as.data.frame(info)
p <- ggplot(infodf) +
  geom_line(aes(x = n, y = mn), color = "red") +
  geom_line(aes(x = n, y = fn), color = "blue") +
  geom_line(aes(x = n, y = fn - mn), color = "green")
plot(p)