## This two functions are used to save computing power needed to inverse 
## matrices. It's done by calling cacheSolve() function on the output of 
## a constructor function makeCacheMatrix(), eg:
#
# matrixX <- matrix(rnorm(16), 4,4,)
# y <- makeCacheMatrix(matrixX)   #1. matrix is saved to and can be retrived later 
#                                 # by $get() method
#                                 #2. A list of 4 elements is returned 
#                                 #   (outputs of: get(), set(), getinverse(), 
#                                 #                setinverse()).        
# cacheSolve(y)           # 1. when called for the first time calculate new 
#                         # new inverse matrix using solve(matrixX) function
#                         # 2. when called another time instead of repeating
#                         # the same calculation for the same matrix, return saved
#                         # result


## stores matrix and inverse of a matrix after it's passed from cacheSolve() 
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## First, check if the inverse matrix has already calculated, 
## Then, return it or calculate and save with makeCacheMatrix(x)$setinverse(inv)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached inverse matrix")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv)
        message("New calculated inverse matrix is:")
        return(inv)
}

