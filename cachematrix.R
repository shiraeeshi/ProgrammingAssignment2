## Put comments here that give an overall description of what your
## functions do

## returns a list of functions to get and set the matrix
## and its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	get <- function() x
	set <- function(m) {
		x <<- m
		inv <<- NULL
	}
	getInv <- function() inv
	setInv <- function(value) inv <<- value
	list(get = get, set = set, 
		getInv = getInv, setInv = setInv)
}


## returns an inverse matrix of the one passed as a parameter,
## caching the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getInv()
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	m <- x$get()
	inv <- solve(m)
	x$setInv(inv)
	inv
}
