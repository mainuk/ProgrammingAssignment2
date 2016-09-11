## 
## 1st commit SHA-1 hash identifier: 


##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        t <- NULL
        set <- function(y) {
                x <<- y
                t <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) t <<- inverse
        getInverse <- function() t
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix"  
## which created with the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        
        t <- x$getInverse()
        if (!is.null(t)) {
                message("in process")
                return(t)
        }
        m <- x$get()
        t <- solve(m, ...)
        x$setInverse(t)
        t
}

## Testing
matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
cacheSolve(matrix)
