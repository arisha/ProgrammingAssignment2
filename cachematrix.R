## Put comments here that give an overall description of what your
## functions do

#makeCacheMatrix creates a list containing a function
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of inverse of the matrix
#4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y)
        {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


#This functions returns the inverse of a matrix. If the inverse
#has already be computed, it will returnt the cache value and skip the computation


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m<- solve(matrix, ...)
        x$setinverse(m)
        m
}
