
fib_dp <- function(n) {
  ## create a table to store the subproblems in of the right size
  fibs <- seq(1,n)
  ## fill in the base cases
  fibs[1] <- 1
  fibs[2] <- 1
  ## fill in the rest
  for(i in seq(3,n)) {
    fibs[i] <- fibs[i-1] + fibs[i-2]
  }
  ## return the final answer
  return(fibs[n])
}


print(fib_dp(60))