## This includes 2 functions.  One creates a special matrix that can cache its inverse
## The other computes the inverse of the special matrix if the matrix has been changed.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL
        SetMatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        GetMatrix <- function() x
        SetInverse <- function(inverse) m <<- inverse
        GetInverse <- function() m
        list(SetMatrix = SetMatrix, GetMatrix = GetMatrix,
             SetInverse = SetInverse,
             GetInverse = GetInverse)
	}




## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

## Return a matrix that is the inverse of 'x'

	m <- x$GetInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$GetMatrix()
        m <- solve(data, ...)
        x$SetInverse(m)
        m


	}
