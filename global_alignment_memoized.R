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


## given two char vectors of equal length (e.g. c("A", "T", "G") 
## and c("A", "-", "G"), returns their score (e.g. 0, -3, or 4, or whatever))
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
  if((length(xin) > 1 & length(yin) >= 1) | (length(xin) >=1 & length(yin) > 1) ) { 
    stop("Doesn't look like a base case. We die.")    
  
  # return an answer for the simplest case, both have length one
  } else if(length(xin) == 1 & length(yin) == 1) {
    answer = list(x = xin, y = yin, 
                  xaln = xin, yaln = yin, 
                  score = score_aln(xin, yin))
    return(answer)
 

  # if x is of length 0, pad it out and return the answer
  } else if(length(yin) == 0) {
    xaligned <- xin
    yaligned <- c()
    for(i in seq(1,length(xin))) {
      yaligned <- c(yaligned, "-")
    }
    answer = list(x = xin, y = yin, 
                  xaln = xaligned, yaln = yaligned, 
                  score = score_aln(xaligned, yaligned))
    return(answer)
    
  # if y is of length 0, pad it out and return the answer
  } else if(length(xin) == 0) {
    xaligned <- c()
    yaligned <- yin
    for(i in seq(1,length(yin))) {
      xaligned <- c(xaligned, "-")
    }
    answer = list(x = xin, y = yin, 
                  xaln = xaligned, yaln = yaligned, 
                  score = score_aln(xaligned, yaligned))
    return(answer)
    
  } 
   # this shouldn't happen:
  stop("How did you even get here? One of the cases should have happend. Die.") 
  
}

ALN_CACHE <<- hash()
global_aln <- function(x, y) {
  thiscall <- str_c(unvec_char(x), ",", unvec_char(y))
  if(has.key(thiscall, ALN_CACHE)) {
    return(ALN_CACHE[[thiscall]])
  }
  
  if(length(x) == 0 | length(y) == 0 | (length(x) == 1 & length(y) == 1)) {
    ALN_CACHE[[thiscall]] <<- base_case(x,y)
   return(ALN_CACHE[[thiscall]])
  #return(base_case(x,y))
  }
  
  px <- x[1:length(x) - 1]
  ex <- x[length(x)]
  py <- y[1:length(y) - 1]
  ey <- y[length(y)]

  
  A <- global_aln(px, py)
  B <- global_aln(c(px, ex), py)
  C <- global_aln(px, c(py, ey))
  
  answera <- list(x = x, y = y, 
                  xaln = c(A[["xaln"]], ex), 
                  yaln = c(A[["yaln"]], ey), 
                  score = A[["score"]] + score_aln(ex, ey))
  answerb <- list(x = x, y = y, 
                  xaln = c(B[["xaln"]], "-"), 
                  yaln = c(B[["yaln"]], ey),
                  score = B[["score"]] + score_aln("-", ey))
  answerc <- list(x = x, y = y, 
                  xaln = c(C[["xaln"]], ex), 
                  yaln = c(C[["yaln"]], "-"),
                  score = C[["score"]] + score_aln(ex, "-"))
  
  ## return the best of the three
  bestanswer <- answera
  bestscore <- answera[["score"]]
  if(answerb[["score"]] > bestscore) {
    bestanswer <- answerb
    bestscore <- answerb[["score"]]
  }
  if(answerc[["score"]] > bestscore) {
    bestanswer <- answerc
    bestscore <- answerc[["score"]]
  }
  
  ALN_CACHE[[thiscall]] <<- bestanswer
  return(bestanswer)
}







# do iiiiit
x <- char_vec("TATCGGACTG")
y <- char_vec("TCTGGTCCCTA")
answer <- global_aln(x, y)
print(answer)

