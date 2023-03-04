## I don't really understand this assignment, but I managed to change the 
## mean to inverse, m to i, and mean() to slove(), so at least it will work.

## My understanding of the following functions are, if the contents of a matrix
## are not changing, it may make sense to cache the value of the inverse so that 
## when we need it again, it can be looked up in the cache rather than recomputed.

## -----------------------------------------------------------------------------
## makeCacheMatrix creates a special "matrix", which is really a list containing 
## a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
          x <<- y
          i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special "matrix" created with the 
## above function. However, it first checks to see if the inverse has already 
## been calculated. If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the data and sets the 
## value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setmean(i)
        i
}
