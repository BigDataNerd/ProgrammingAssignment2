# Matrix inversion is computationally expensive.  In order to eliminate
# redundant evaluation, the functions in this module will allow the user to
# create a matrix that, upon calculation, caches its inverse and returns that
# inverse on subsequent queries until the original matrix changes (invalidating
# the cached matrix)

# makeCacheMatrix returns a list containing a matrix that is capable of caching
# its inverse in order to eliminate redundant and unnecessary computation.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL   # cached value of inverse, if exists

	set <- function(y) {
		i <<- NULL
		x <<- y
	}
	
	get <- function() x
	
	getinverse <- function() i
	
	setinverse <- function(inverse) i <<- inverse
	
	list(get=get, set=set, getinverse=getinverse, setinverse=setinverse)
}


# cacheSolve will return the inverse of matrix contained within its
# first argument.  Upon first calculation, the 'solve' function is called to
# calculate the returned inverse and cache it; subsequent calls return the cached value.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if(!is.null(i)) {
		# return cached inverse...
		message('returning cached inverse')
		i
	}
	
	# no cached inverse, so calculate it using underlying data and retain, return
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
