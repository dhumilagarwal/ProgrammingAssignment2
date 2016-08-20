## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function creates a matrix 

makeCacheMatrix <- function(actual_matrix = matrix()) {
  invert <- NULL
  set <- function(new_matrix) {
    actual_matrix <<- new_matrix
    invert <<- NULL
  }
  get <- function() actual_matrix
  set_invert <- function(new_invert) invert <<- new_invert
  get_invert <- function() invert
  list(set = set, get = get,
       set_invert = set_invert,
       get_invert = get_invert)
}


## Write a short comment describing this function

cacheSolve <- function(actual_matrix, ...) {
        ## Return a matrix that is the inverse of 'x'
  
   invert <- as.list(actual_matrix)$get_invert()
  if(!is.null(invert)) {
    message("getting cached data")
    return(invert)
  }
  data <- actual_matrix$get()
  invert <- solve(data, ...)
  actual_matrix$set_invert(invert)
  invert
}

# Create Sample Matrix
my_sample_matrix <- matrix(data = sample(100,size = 64, replace = TRUE), nrow = 8, ncol = 8)
my_sample_matrix

# Get the Inverted Matrix
my_inverted_matrix <- solve(my_sample_matrix)
my_inverted_matrix

# Test the cacheSolve function and makeCacheMatrix function
makeCacheMatrix(my_sample_matrix)
my_updated_invert <- cacheSolve(my_sample_matrix)
my_updated_invert == my_inverted_matrix