

quicksort <- function(elements) {
  if(length(elements) <= 1) {
    return(elements)
  }
  
  ## "regular" quicksort picks a simple (hopefully random)
  ## pivot
  #pivot <- sample(elements, 1)
  pivot <- median(elements) # work: n (if using linear-time median alg)
  less <- elements[elements < pivot] # work: n
  eq <- elements[elements == pivot]  # work: n
  greater <- elements[elements > pivot] # work: n
  
  less_sorted <- quicksort(less)
  greater_sorted <- quicksort(greater)
  
  answer <- c(less_sorted, eq, greater_sorted) # work n
  return(answer)
}


sample <- as.integer(runif(10000, 1, 1000))
print(head(sample))
sorted <- quicksort(sample)
print(head(sorted))