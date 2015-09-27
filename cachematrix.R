## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function returns a special matrix object that can
# cache its inverse. It returns a list of functions.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # Allows the user to set the matrix to be retrieved
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # Returns the matrix passed to the function
  get <- function() x
  # Allows the user to cache the inverse of the matrix
  setinverse <- function(solve) m <<- solve
  # Returns the cached inverse
  getinverse <- function() m
  # Returns the above functions for use in cacheSolve or otherwise
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# This function returns the cached inverse matrix if it exists,
# otherwise it calculates the inverse of the special matrix object
# passed to it.
cacheSolve <- function(x, ...) {
  # m is set to be the cached inverse if it exists.
  m <- x$getinverse()
  if(!is.null(m)) {
    # If there is cached data, a message is printed and the
    # cached data is returned
    message("getting cached data")
    return(m)
  }
  # Otherwise the stored matrix is moved into the data object
  data <- x$get()
  # Then the inverse of the matrix is computed
  m <- solve(data, ...)
  # The inverse is cached
  x$setinverse(m)
  # The inverse is returned
  m
}

# SANDBOX***************************************************************
# This area contains code that can test the above functions.
# Creates sequence
seq1 <- seq(1:4)
# Stores in 2x2 matrix
matrix1 <- matrix(seq1, 2)
# Makes a a 'special' matrix
a <- makeCacheMatrix(matrix1)
# Sets the matrix to be retrieved to matrix1
a$set(matrix1)
# Checks that the correct matrix is returned
a$get()
# Stores the inverse of the matrix
a$setinverse(solve(matrix1))
# Returns the inverse of the matrix
a$getinverse()

# Running cacheSolve on a, the special matrix representation
# of matrix1 returns the cached data!
cacheSolve(a)

# Note that if we create a matrix b, the inverse of matrix a
# is still returned because that is what has been cached.
seq2 <- seq(5:8)
matrix2 <- matrix(seq2, 2)
b <- makeCacheMatrix(matrix2)
cacheSolve(b)