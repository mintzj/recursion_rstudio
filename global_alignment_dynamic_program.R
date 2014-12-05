
library(stringr)
library(rstackdeque)
library(hash)
library(ggplot2)
library(grid)

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



global_aln <- function(x, y) {
  x <- c("", x)
  y <- c("", y)
  scores <- matrix(0, ncol = length(x), nrow = length(y))
  arrows <- matrix("?", ncol = length(x), nrow = length(y))
  
  ## Base cases along the top
  row <- 1
  for(col in seq(2, length(x))) {
    scores[row, col] <- scores[row, col - 1] + score_aln("-", x[col])
    arrows[row, col] <- "left"
  }
  
  ## Base cases along the side
  col <- 1
  for(row in seq(2, length(y))) {
    scores[row, 1] <- scores[row - 1, 1] + score_aln(y[row], "-")
    arrows[row, 1] <- "up"
  }
  
  ## Fill in rest of table in left-to-right, top-to-bottom order
  for(col in seq(2, length(x))) {
    for(row in seq(2, length(y))) {
      diagscore <- scores[row - 1, col - 1] + score_aln(x[col], y[row])
      leftscore <- scores[row, col - 1] + score_aln(y[row], "-")
      upscore <- scores[row - 1, col] + score_aln("-", x[col])

      bestscore <- diagscore
      bestfrom <- "diag"
      if(leftscore > bestscore) {
        bestscore <- leftscore
        bestfrom <- "left"
      }
      if(upscore > bestscore) {
        bestscore <- upscore
        bestfrom <- "up"
      }
      scores[row, col] <- bestscore
      arrows[row, col] <- bestfrom
      
    }
  }
  
  
  # now the traceback, starting in the lower right
  currentrow <- length(y)
  currentcol <- length(x)
  alnx <- c("")
  alny <- c("")

  # until we get to the upper left
  while(currentrow != 1 & currentcol != 1) {
    from <- arrows[currentrow, currentcol]
    if(from == "diag") {
      alnx <- c(alnx, x[currentcol]) # align...
      alny <- c(alny, y[currentrow])
      currentrow <- currentrow - 1   # and move
      currentcol <- currentcol - 1
    } else if(from == "left") {
      alnx <- c(alnx, x[currentcol]) # align...
      alny <- c(alny, "-")
      currentcol <- currentcol - 1   # and move
    } else if(from == "up") {
      alnx <- c(alnx, "-")           # align...
      alny <- c(alny, y[currentrow])
      currentrow <- currentrow - 1   # and move
    }
  }
  
  answer <- list(alnx = rev(alnx), 
                 alny = rev(alny))
  return(answer)
}



x <- char_vec("TATCTGCAACGA")
y <- char_vec("TTGTGC")
answer <- global_aln(x, y)
print(answer)

