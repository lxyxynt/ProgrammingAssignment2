# To create a special matrix object with ability to cache its inverse

# makeCacheMatrix is a constructor function which generates a special 
# matrix object with functions:
# - set: set the value of the matrix
# - get: get the value of the matrix
# - setInv: set the inverse of the matrix
# - getInv: get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inv) inv <<- inv
	getinv <- function() inv
	list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

# cacheSolve will calculate the inverse of the matrix object
# if the value has been calculated and cached before, it will 
# just return the cached value.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    message("calculating inverse")
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinv(inv)
    inv
}