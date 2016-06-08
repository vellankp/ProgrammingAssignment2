## Computes the inverse of a matrix and caches the answer 
## 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  invM <- NULL
  
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(i) invM <<- i
  getInverse <- function() invM
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  invM <- x$getInverse()
  
  if (!is.null(invM)) {
    message("getting cached data")
    return(invM)
  }
  
  data <- x$get()
  invM <- solve(data, ...)
  x$setInverse(invM)
  invM
}


