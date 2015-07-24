## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute it repeatedly 

## makeCacheMatrix creates a list containing a function
## 1. to set the value of the matrix
## 2. will get the value of the matrix
## 3. to set the value of inverse of the matrix
## 4. will get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, 
             setinverse=setinverse, 
             getinverse=getinverse)
}


## This function computes the inverse of the special 
##"matrix" returned by makeCacheMatrix above. 

##If the inverse has already been calculated 
##(and the matrix has not changed), then cacheSolve should 
##retrieve the inverse from the cache.

##assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}








