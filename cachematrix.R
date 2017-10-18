## This is a functions that cache the inverse of a matrix

## To creates a special matrix object that caches its inverse

makeCacheMatrix <- function( m = matrix() ) {
  
  ## to initialize the inverse property
  ini <- NULL
  
  ## to set the matrix
  Set <- function( matrix ) {
    m <<- matrix
    ini <<- NULL
  }
  
  ##to get the matrix
  get <- function() {
    ## Return the matrix
    m
  }
  
  ## to set the inverse of the matrix
  setInverse <- function(inverse) {
    ini <<- inverse
  }
  
  ## to get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    ini
  }
  
  ## Return a list of the methods
  list(Set = Set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## To compute the inverse of the special matrix returned by the "makeCacheMatrix"
## function above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Just return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return the matrix
  m
}
