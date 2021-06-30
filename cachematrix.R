## Creates the cached matrix, sets the matrix data, gets the matrix data and gets the cached matrix

makeCacheMatrix <- function(x = matrix()) 
  {
  
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }



## Calculates the inverse of matrix if its the same matrix/ object else returns the cached data

cacheSolve <- function(x, ...) 
  {
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
  }
