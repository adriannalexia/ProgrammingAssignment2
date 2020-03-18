## The functions take a square matrix, 
## store the inverse of the matrix in the cache. 


## makeCacheMatrix creates a list of functions and returns said list.
## cacheSolve then takes the list of functions to get or set
## the inverted matrix in the cache. 

makeCacheMatrix <- function(x = matrix()) {
  # stores the cached value and initializes it to NULL
  cache_inv <- NULL
  
  # creates matrix in working environment
  set <- function(y){
    x <<- y
    cache_inv <<- NULL
  }
  
  # get value of matrix
  get <- function() x
  # inverts the matrix and stores it in the cache
  setinv <- function(inverse) cache_inv <<- inverse
  # gets the inverted matrix from the cache
  getinv <- function() cache_inv
  # return created fxns to the working environment
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve calculates the inverse of the matrix
## If the cache is empty, the inverse of the matrix is 
## calculated in the working environment and stored in
## in the cache

cacheSolve <- function(x, ...) {
  # If cache is not empty: retrieve and return inverse 
  # of the matrix stored in the cache
  cache_inv <- x$getinv()
  if(!is.null(cache_inv)) {
    message("getting cached matrix")
    return(cache_inv)
  }
  # create matrix since it does not exist
  matrix <- x$get()
  # invert matrix
  cache_inv <- solve(matrix, ...)
  # set inverted matrix in cache
  x$setinv(cache_inv)
  # return inverse matrix in console
  cache_inv
}
