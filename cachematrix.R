## makeCacheMatrix: This function creates a special matrix object " invMtrx" that can cache its inverse
## makeCacheMatrix: This function Orgmtrx as a Original Matrix provided as input.

makeCacheMatrix <- function(Orgmtrx = matrix()) {
  invMtrx <- NULL
  
  set <- function(y) {
        Orgmtrx <<- y
        invMtrx <<- NULL
  }
  
  get <- function() { return(Orgmtrx) }
  
  setInverse <- function(inverse){invMtrx <<- inverse}
  
  getInverse <- function(){ return(invMtrx) }
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
## First the function checks for if X is a square invertible matrix, then only attemp to solve(X) returns its inverse.

cacheSolve <- function(mtrx , ... ) {
  
  invMtrx <- mtrx$getInverse()
  mat <- mtrx$get()
  
  if(det(mat) == 0) {
    message("Inverse of the given matrices is not feasibe")
    return()
  } else if(!is.null(mat)){
                message("getting cached data")
                return(mat)
        }
  
  invMtrx <- solve(mat, ...)
  mtrx$setInverse(invMtrx)
  invMtrx
}


## Usage for invertible matrix
## my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
## my_matrix$get()
## my_matrix$getInverse()
## cacheSolve(my_matrix)
## my_matrix$getInverse()

## Usage for non invertible matrix
## my_matrix <- makeCacheMatrix(matrix(1:9,  3, 3))
## my_matrix$get()
## my_matrix$getInverse()
## cacheSolve(my_matrix)
## my_matrix$getInverse()
