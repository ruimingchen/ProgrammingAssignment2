## Function makeCacheMatrix creates a special "matrix", which is a list 
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setmatrix <- function(y) {
                x <<- y
                inv <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(setmatrix = setmatrix, getmatrix = getmatrix,
        setinverse = setinverse,
        getinverse = getinverse)
}

## Function cacheSolve calculates the inverse of the special "matrix" created 
## with the above function makeCacheMatrix. It first checks to see if the
## inverse has already been calculated. If so, it gets the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse
## of the data and sets the value of the inverse in the cache via the 
## setinverse function

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$getmatrix()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
