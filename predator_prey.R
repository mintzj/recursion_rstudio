
## Based on http://www.mnkjournals.com/ijlrst_files/download/Vol%202%20Issue%201/12-5064-M.Reni%20Sagaya%20Raj.pdf
## Specifically figure 4a
library(rstackdeque)
library(ggplot2)
library(hash)

PREY_CACHE <<- hash()
prey <- function(t, r, b, c, d) {
  thiscall <- str_c(t, ":", r, ":", b, ":", c, ":")
  if(has.key(thiscall, PREY_CACHE)) {
    return(PREY_CACHE[[thiscall]])
  }
  
  if(t == 1) {
    PREY_CACHE[[thiscall]] <<- 0.8
    return(0.8)
  } else {
    answer <- (r+1)*prey(t - 1, r, b, c, d) - r*prey(t-1, r, b, c, d)^2 - b*prey(t-1, r, b, c, d)*predators(t-1, r, b, c, d)
    PREY_CACHE[[thiscall]] <<- answer
    return(answer)
  }
}

PREDATORS_CACHE <<- hash()
predators <- function(t, r, b, c, d) {
  thiscall <- str_c(t, ":", r, ":", b, ":", c, ":")
  if(has.key(thiscall, PREDATORS_CACHE)) {
    return(PREDATORS_CACHE[[thiscall]])
  }
  
  if(t == 1) {
    PREDATORS_CACHE[[thiscall]] <<- 0.3
    return(0.3)
  } else {
    answer <- c*prey(t-1, r, b, c, d)*predators(t-1, r, b, c, d) + (1-d)*predators(t-1, r, b, c, d) 
    PREDATORS_CACHE[[thiscall]] <<- answer
    return(answer)
  }
}

info <- rstack()
for(i in seq(1,300)) {
  info <- insert_top(info, list(t = i, prey = prey(i, 0.25, 1.1, 0.95, 0.55), predators = predators(i, 0.25, 1.1, 0.95, 0.55)))
}

infodf <- as.data.frame(info)
p <- ggplot(infodf) +
  geom_line(aes(x = t, y = prey), color = "blue") +
  geom_line(aes(x = t, y = predators), color = "red")

plot(p)
  
  