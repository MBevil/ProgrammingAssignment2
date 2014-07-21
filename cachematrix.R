## cachematrix.r
## Code for R Programming - Programming Assignment #2 
## The will utilize lexical scoping to allow the caching of a time 
## intensive operation - in this case the computed inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

## test data: z <- stats::rnorm(16)
##            dim(z) <- c(4,4)

makeCacheMatrix <- function(x = matrix()) {
  
  inv_x <- NULL
  
  set <- function(y) {
    # set the value in the cache
    x <<- y
    ## since this has just been set clear the inverse value in the cache
    ## as it will need to be recalulated
    inv_x <<- NULL
  }
  
  get <- function() {
    ## return the cached value
    return(x)
  }
  
  setinverse<- function(inverse) {
    ## set the inverse value in the cache
    inv_x <<-inverse
  }
  
  getinverse <- function() {
    ## get the inverse value in the cache
    return(inv_x)
  }
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## get the cached inverse value
  inv_x <- x$getinverse()
  
  ## Check if the inverse value has been calulated (e.g. not null)
  if (!is.null(inv_x)) {
    message("getting cached data")
  } else {
    message("calculating inverse")
    ## calculate the inverse value
    ## X is a square invertible matrix, then solve(X) returns its inverse.
    inv_x <- solve(x$get())
    ## save the calculated value
    x$setinverse(inv_x)  
  }
  #return the value
  return(inv_x)
}
