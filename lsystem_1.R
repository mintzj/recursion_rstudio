library(rstackdeque)
library(TurtleGraphics)
library(stringr)
library(hash)



sentence <- "M"

rules <- hash()
rules[["M"]] <- c("F")
rules[["F"]] <- c("M", "F")


lproduce <- function(sentence, rules) {
  newsentence <- c()
  for(symbol in sentence) {
    if(has.key(symbol, rules)) {
      ## if theres a rule about this symbol, replace with the symbol
      newsentence <- c(newsentence, rules[[symbol]])
    } else {
      ## otherwise, leave the symbol alone
      newsentence <- c(newsentence, symbol)
    }
  }
  return(newsentence)
}

print(sentence)
sentence <- lproduce(sentence, rules)
print(sentence)
sentence <- lproduce(sentence, rules)
print(sentence)
sentence <- lproduce(sentence, rules)
print(sentence)
sentence <- lproduce(sentence, rules)
print(sentence)
sentence <- lproduce(sentence, rules)
print(sentence)
sentence <- lproduce(sentence, rules)
print(sentence)
sentence <- lproduce(sentence, rules)


