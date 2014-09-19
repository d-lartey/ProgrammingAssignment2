## The purpose of the functions here is to cache the inverse of a Matrix

## makeCacheMatrix is a function that creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse_of_x <- NULL
        set <- function(y){
                x <<- y
                inverse_of_x <<- NULL
        }
        get <- function() x
        
        setinverse <- function(solve) inverse_of_x <<-solve
        getinverse <- function () inverse_of_x
        
        list(set=set, 
             get=get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed)
## then the function should retrieve the inverse from the cache.

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inverse_of_x <- x$getinverse()
        if(!is.null(inverse_of_x)) {
                return(inverse_of_x)
        }
        
        data <- x$get()
        inverse_of_x <- solve(data)
        x$setinverse(inverse_of_x)
        inverse_of_x
}
