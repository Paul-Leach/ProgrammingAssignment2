## makeCacheMatrix  -- constructor for matrix object with cacheable inverse

makeCacheMatrix <- function(x = matrix()) {
  # m is the current value of the inverse if one has been set; initially NULL
  m <- NULL
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # get the value of the matrix
  get <- function() x
  # set the inverse of the matrix
  setinv <- function(inv) m <<- inv
  # get the inverse of the matrix
  getinv <- function() m
  
  # return the list of funtions to manipulate the matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve -- Return a matrix that is the inverse of 'x'
##  using a cached copy if available
cacheSolve <- function(x, ...) { 

  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
 
