library(ggplot2)
library(rstackdeque)
library(hash)
library(stringr)

Q_CACHE <- hash()

q <- function(n) {
  thiscall <- str_c(n)
  if(has.key(thiscall, Q_CACHE)) {
    return(Q_CACHE[[thiscall]])
  }
  if(n == 1 | n == 2) {
    return(1)
  } else {
    answer <- q(n - q(n-1)) + q(n - q(n-2))
    Q_CACHE[[thiscall]] <- answer
    return(answer)
  }
}

info <- rstack()
for(i in seq(1,10000)) {
  row <- list(n = i, qn = q(i))
  info <- insert_top(info, row)
}

infodf <- as.data.frame(info)
p <- ggplot(infodf) +
  geom_line(aes(x = n, y = qn - n/2))

plot(p)
