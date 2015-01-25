## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix create object that allow caching of inversion of given matrix.
# `get` method return stored matrix
# `set` method set new matrix
#
# m <- makeCacheMatrix(matrix(1:4, 2, 2))
# d <- m$get()
# m$set(matrix(5:8, 2, 2))

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	xsaved <- x
	
	set <- function(y) {
		x <<- y
		xsaved <<- y
		inv <<- NULL
	}
	
	get <- function() { x }
	
	setinv <- function(invn) {
		inv <<- invn
		xsaved <<- x
	}
	
	getinv <- function() { inv }
	
	updated <- function() { ! identical(x, xsaved) }
	
	list(set = set, get = get
	     , setinv = setinv, getinv = getinv, updated = updated)
}


## Write a short comment describing this function

# cacheSolve get object from makeCacheMatrix and calculate its inversion
# invm <- cacheSolve(m)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)) {
		
		if(!x$updated()) {
			message("getting cached data")
			return(inv)
		} else {
			message("matrix was changed")
		}
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}
