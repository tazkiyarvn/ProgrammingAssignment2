## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  cache = NULL
  setMatrix <- function(matrix){
    cache$matrix <<- matrix
    cache$inverse <<- NULL
  }
  getMatrix <- function() {
    cache$matrix
  }
  setInverse <- function(inv) cache <<- inv
  getInverse <- function(){
    if(!is.null(cache$inverse)){
      message("Getting cached inverse")
      return(cache$inverse)
    }else{
      message("Calculating and caching inverse")
      inv <- solve(cache$matrix)
      cache$inverse <- inv
      return(inv)
    }
  }
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(cacheMatrix) {
        ## Return a matrix that is the inverse of 'x'
  if (is.null(cacheMatrix$getMatrix())){
    stop("Matrix is not set. Use setMatrix() first")
  }
  
  cachedInverse <- cacheMatrix$getInverse()
  
  if (!is.null(cachedInverse)){
    return(cachedInverse)
  } else{
    matrix <- cacheMatrix$getMatrix()
    inv <- solve(matrix)
    cacheMatrix$setMatrix(matrix)
    cacheMatrix$getInverse()
  }
}
