# Coursera R programming 
# Programming assigment week 3

# Create a special "matrix", which is a list containing
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse matrix
# 4. get the value of the inverse matrix

#  Example

#  > mat <- makeCacheMatrix(matrix(c(2,4,3,7),c(2,2)))
#  > cacheSolve(mat)
#  Retreiving the cached inverse matrix...
#       [,1] [,2]
#  [1,]  3.5 -1.5
#  [2,] -2.0  1.0

# The following two functions are used to cache the inverse of a matrix.
# makeCacheMatrix: This function creates a special "matrix" object that 
# can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y 
    i <<- NULL  
  }
  get <- function() x
  set_inv <- function(inv) i <<- inv
  get_inv <- function() i
  list(set = set,  get = get, set_inv = set_inv,
       get_inv = get_inv)    
}

# Following funcion takes the matrix stored by the previous function and 
# then returns its inverse. If the inverse of the cached matrix
# has already been solved, the function 
# uses the cached data

cacheSolve <- function(x, ...) {
  
  i <- x$get_inv()
  
  if(!is.null(i)) {
    message("Retreiving the cached inverse matrix...")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$set_inv(i)
  i
}
