## makeCacheMatrix is a function that does 4 functions; 
## setting the value of the matrix, getting the matrix,
## setting the value of the inverse matrix, and getting it.

makeCacheMatrix <- function(x = matrix()) {
  cache = NULL
  setMatrix <- function(y){
    x <<- y
    cache <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(y) cache <<- y
  getInverse <- function()y
  list(setMatrix = setMatrix, getMatrix = getMatrix, getInverse = getInverse)
}


## cacheSolve checks whether the inverse value has been set or not
## if it has been set, it will return the value, but if it has not,
## it will compute and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  if (!is.null(cache)){
    message("getting cached data")
    return(cache)
  } else{
    matrix <- cacheMatrix$getMatrix()
    cache <- solve(matrix)
    cacheMatrix$setMatrix(matrix)
    cacheMatrix$getInverse()
  }
}
