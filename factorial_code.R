#--Load packages
library(purrr)

#--Factorial loop
factorial_loop <- function(n) {
  stopifnot(n >= 0)

    #--The factorial of 0 is defined to be 1
  if (n == 0){
    result <- 1
  } else{
    result <- 1
      
    for (i in 1:n){
      result <- result * i
      }
    }
  return(result)
}

#purrr::map_int(0, factorial_loop)

#--Factorial reduce
factorial_reduce <- function(n){
  stopifnot(n >= 0)
  
  #--The factorial of 0 is defined to be 1
  if (n == 0){
    result <- 1
  } else{
    result <- 1
      
    result <- purrr::reduce(n:1, function(x, y){
      x * y})
    }
  return(result)
}

#--Factorial recursion
factorial_func <- function(n){
  stopifnot(n >= 0)
  
  if (n == 0){
    result <- 1
  } else {
    result <- n * factorial_func(n - 1)
  }
  
  return(result)
}

#--Factorial memoization
#-Create a lookup table to store the computed results
factorial_tbl <- c(0, 1, rep(NA, 100))

#-Define the memoized factorial function
factorial_mem <- function(n) {
  stopifnot(n >= 0)

  if (n == factorial_tbl[1]){
    return(1)
  } else {
    # Calculate the factorial recursively and store the result in the lookup table
    factorial_tbl[n] <<- factorial_mem(n - 1) * n
    return(factorial_tbl[n])
  }
}