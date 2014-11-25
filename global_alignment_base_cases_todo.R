library(stringr)
library(rstackdeque)
library(hash)

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

## given two char vectors of equal length (e.g. c("A", "T", "G") and c("A", "-", "G"),
## returns their score (e.g. 0, -3, or 4, or whatever))
score_aln <- function(xin, yin) {
  if(length(xin) != length(yin)) {
    stop("Can't score two sequences of unequal length. We die.")
  }
  
  score = 0
  for(i in seq(1,length(xin))) {
    xi = xin[i]
    yi = yin[i]
    if(xi == yi) {                     # match: + 2
      score <- score + 2
    } else if(xi == "-" | yi == "-") { # gap: -4
      score <- score - 4
    } else {                           # mismatch: -3
      score <- score - 3
    }
  }

  return(score)
}


## Takes character vectors x and y (as in c("A", "C", "T") or c() or c("G"))
## and check to see if they can be a base-case; if so returns an answer list
base_case <- function(xin, yin) {
  # die if this doesn't look like a base case
  if(length(xin) >= 1 & length(yin) > 1) {
    stop("This isn't a base case. We die.")  
  } else if(length(yin) >= 1 & length(xin) > 1) {
    stop("This isn't a base case either! We die.")
        
  
  # return an answer for the simplest case, both have length one
  } else if(length(xin) == 1 & length(yin) == 1) {
    answer = list(x = xin, y = yin, xaln = xin, yaln = yin, score = score_aln(xin, yin))
    return(answer)
    
  # if x is of length 0, pad it out and return the answer
  } else if(length(yin) == 0) {
    xaligned <- xin
    yaligned <- c()
    for(i in seq(1,length(xin))) {
      yaligned <- c(yaligned, "-")
    }
    answer = list(x = xin, y = yin, xaln = xaligned, yaln = yaligned, score = score_aln(xaligned, yaligned))
    return(answer)
    
  # if y is of length 0, pad it out and return the answer
  } else if(length(y) == 0) {
    # TODO: do the remaining base case
    
    
  # this shouldn't happen:
  } else {
    stop("How did you even get here? We die.") 
  }
  
}



### We're going to format an "answer" as list, with in the input and the output
### specified, as well as the storing the score
answer <- list(x = c("T", "A", "C"), 
               y = c("T", "G"), 
               xaln = c("T", "A", "C"), 
               yaln = c("T", "-", "G"),
               score = -5)


### Checking a base case
xpre <- "TAC"
ypre <- ""
x <- char_vec(xpre)
y <- char_vec(ypre)

base_case_answer_1 <- base_case(x, y)
print(base_case_answer_1)


### Checking another base case
xpre <- "T"
ypre <- "A"
x <- char_vec(xpre)
y <- char_vec(ypre)

base_case_answer_2 <- base_case(x, y)
print(base_case_answer_2)



