## This is a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix is a function that creates a special "matrix" object
##   that can cache its inverse.

## cacheSolve is a function that computes the inverse of the special "matrix"
##   returned by makeCacheMatrix above. If the inverse has already been calculated
##   (and the matrix has not changed), then the cachesolve should retrieve the inverse
##   from the cache.
###################################

## makeCacheMatrix is a function that creates a special "matrix" object
##   that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<-NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<-inverse
    getinv <- function() inv
    list (set=set, get=get,setinv = setinv, getinv=getinv)
}


## cacheSolve is a function that computes the inverse of the special "matrix"
##   returned by makeCacheMatrix above. If the inverse has already been calculated
##   (and the matrix has not changed), then the cachesolve should retrieve the inverse
##   from the cache.

cacheSolve <- function(x, ...) {
        inv <-x$getinv()
        if(!is.null(inv)){
          message("getting cached inverse matrix")
          return(inv)
        } else {
            inv <- solve(x$get())
            x$setinv(inv)
            return(inv)
        }
}
