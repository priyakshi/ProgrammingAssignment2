## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    invs = NULL
    set = function(y) {
  # use `<<-` to assign a value to an object in an environment different from the current environment. 
      x <<- y
      invs <<- NULL
    }
    get = function() x
    setinv = function(inverse) invs <<- inverse 
    getinv = function() invs
    list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}


## Write a short comment describing this function
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.


  cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    invs = x$getinv()
    
    # if the inverse has already been calculated
    if (!is.null(invs)){
      # get it from the cache and skips the computation. 
      message("getting cached data")
      return(invs)
    }
    
    # otherwise, calculates the inverse 
    mat.data = x$get()
    invs = solve(mat.data, ...)
    
    # sets the value of the inverse in the cache via the setinv function.
    x$setinv(invs)
    
    return(invs)
    
  
}

