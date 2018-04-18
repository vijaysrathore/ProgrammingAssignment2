## makeCacheMatrix: This function creates a special matrix object " invMtrx" that can cache its inverse
## makeCacheMatrix: This function Orgmtrx as a Original Matrix provided as input.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}


## Usage for invertible matrix
## my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
## my_matrix$get()
## my_matrix$getInverse()
## cacheSolve(my_matrix)
## my_matrix$getInverse()



