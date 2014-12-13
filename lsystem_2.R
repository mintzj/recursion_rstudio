library(rstackdeque)
library(TurtleGraphics)
library(stringr)
library(hash)



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





## Given a charater vector of length 1, returns a character 
## vector of characters (of length nchar(string)). E.g.
## c("ACTAG") => C("A", "C", "T", "A", "G")
char_vec <- function(string) {
  # if the string is just c(""), return a 0-length vector
  # (just NULL in R, ugh)
  if(string == "" & length(string) == 1) {
    return(c())
  }
  c_vec <- str_split(string, "")[[1]]
  c_vec <- c_vec[seq(2,length(c_vec))]
  return(c_vec)
}

## given a vector of single character letters, joins them into a single string
## c("A", "C", "T", "A", "G") => c("ACTAG")
unvec_char <- function(a) {
  ret <- str_c(c(a), collapse = "")
  return(ret)
}



# returns a vector that remembers the turtles position and angle at the time
# it's called
turtle_getstate <- function() {
  state <- c(turtle_getpos(), turtle_getangle())
  return(state)
}

# sets the turtles position and angle according to a state vector produced by getstate
turtle_setstate <- function(s) {
  turtle_setpos(s[1], s[2])
  turtle_setangle(s[3])
}


draw_sentence <- function(sentence, size, langle, rangle) {
  pos_stack <- rstack()
  for(symbol in sentence) {
    if(symbol == "F") {
      turtle_forward(size)
      #size <- size * 0.99
    } else if(symbol == "-") {
      turtle_turn(langle, "left")
    } else if(symbol == "+") {
      turtle_turn(rangle, "right")
    } else if(symbol == "[") {
      pos_stack <- insert_top(pos_stack, turtle_getstate())
      #size <- size * (2/3)
    } else if(symbol == "]") {
      turtle_setstate(peek_top(pos_stack))
      pos_stack <- without_top(pos_stack)
      #size <- size / (2/3)
    }
  }
}





sentence <- char_vec("F[-F]F[+F]F")

turtle_init(mode = "clip")
turtle_hide()
turtle_setstate(c(50,0,0))  # x, y, angle

draw_sentence(sentence, 10, 30, 30)










##############################

sentence <- char_vec("F[-F]F[+F]F")

rules <- hash()
rules[["F"]] <- char_vec("F[-F]F[+F]F")

sentence <- lproduce(sentence, rules)
sentence <- lproduce(sentence, rules)
print(unvec_char(sentence))


turtle_init(mode = "clip")
turtle_hide()
turtle_setstate(c(50,0,0))  # x, y, angle

draw_sentence(sentence, 5, 30, 30)





###############################

sentence <- "F"

rules <- hash()
rules[["F"]] <- char_vec("F[-F]F[+F]F")

for(i in seq(1,5)) {
  sentence <- lproduce(sentence, rules)
}

turtle_init(mode = "clip")
turtle_hide()
turtle_setstate(c(50,0,0))  # x, y, angle

draw_sentence(sentence, 5, 30, 30)









################################





