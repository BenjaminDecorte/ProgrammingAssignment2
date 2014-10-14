## This function contains some functions to help to use the cache for 
## the inverse of a matrix
## Decorte Benjamin - 2014-10-14
makeCacheMatrix <- function(m = matrix()) {
  ##Initialize the Inverse
  inv <- NULL
  ##Set the matrix
  set <- function(matrix) {
    m <<- matrix
    inv <<- NULL
  }
  ## function to get the matrix
  get <- function() 
  {
    m
  }
  ## Set the Inverse
  setInv <- function(Inverse) 
  {
    inv <<- Inverse
  }
  ## function to get the inverse
    getInv <- function() 
  {
    inv
  }
  ## return the list of available methods
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## Function to compute the Inverse of a matrix.
## If the Inverse has already been calculated, then cacheSolve could 
## retrieve the inverse from the cache; If not, we store the Inverse in the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  ## If already calculated, return the Inverse from the cache and stop here
  if( !is.null(m) ) 
    {
    message("getting cached data")
    return(m)
  }
  ## Get the matrix 
  data <- x$get()
  ## Calculate the inverse 
  m <- solve(data) %*% data
  ## Set the inverse 
  x$setInv(m)
  ## Return the matrix
  m  
}