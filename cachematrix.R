# Programming Assignment 2: Cache the inverse of  matrix
# Matrix inversion is usually a costly computation and there may be some benefit to 
# caching the inverse of a matrix rather than compute it repeatedly.
# Below are a pair of functions that cache the inverse of a matrix.
# For this assignment, we assume that the matrix supplied is always invertible.
#
# Computing the inverse of a square matrix can be done with the solve function in R. 
# For example, if X is a square invertible matrix, then solve(X) returns its inverse.

# Test on        which has inverse
#  4  3            -2  3
#  3  2             3 -4
#
# by first sourcing this file in R (source path.to.file/cachematrix.R
# then
#  my_matrix <- makeCacheMatrix(matrix(rep(0,4), 2, 2))
#  my_matrix$get()
#  my_matrix$set(matrix(c(4, 3, 3, 2), 2, 2))
#  my_matrix$get()
#  my_matrix$getinverse()
#  cacheSolve(my_matrix)
#  cacheSolve(my_matrix)
#  my_matrix$getinverse()


# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m_inv <- NULL
        set <- function(y) {
                x <<- y
                m_inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m_inv <<- inverse
        getinverse <- function() m_inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# cacheSolve: This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
# has not changed), then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_inv <- x$getinverse()
        if(!is.null(m_inv)) {
                message("getting cached data")
                return(m_inv)
        }
        mat <- x$get()
        m_inv <- solve(mat, ...)
        x$setinverse(m_inv)
        m_inv
}

