## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
#makeCacheMatrix creates a 'matrix' function that cache its inverse
  inv <- NULL
  ## Set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
   ## Get the matrix
  get <- function() x
  ## Set the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  ## Get the inverse of the matrix
  ## Return a list of the functions outputs
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
#cacheSolve function computes the inverse of matrix created by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
## then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  mat <- x$get()
     ## Get the matrix from our object
  inv <- solve(mat, ...)
  x$setInverse(inv)
   ## Return the matrix
  inv
}


#my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
#my_matrix$get()
#     [,1] [,2]
#[1,]    2    1
#[2,]    2    4

#cacheSolve(my_matrix)
#           [,1]       [,2]
#[1,]  0.6666667 -0.1666667
#[2,] -0.3333333  0.3333333

#my_matrix$getInverse()
#           [,1]       [,2]
#[1,]  0.6666667 -0.1666667
#[2,] -0.3333333  0.3333333

