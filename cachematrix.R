## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. 
## The following functions enable caching the inverse of a matrix

## makeCacheMatrix creates a list of functions for
## 1. setting the matrix
## 2. getting the matrix
## 3. setting the inverse of the matrix
## 4. getting the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(matrix) {
                x <<- matrix
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(matrixInverse) inverse <<- matrixInverse
        getInverse <- function() inverse
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve returns the inverse of a matrix either from cache or computes it from scratch.
## Function first checks if the inverse of a matrix is already computed and set. If the inverse is not NULL 
## (meaning inverse has been already computed and set), the cached value is returned.
## Otherwise, the inverse of the matrix is computed with solve() function and the computed 
## value is set to the list object with setInverse() function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if (!is.null(inverse)) {
                message("getting cached inverse matrix")
                return(inverse)
        }
        matrix <- x$get()
        inverse <- solve(matrix)
        x$setInverse(inverse)
        inverse
}
