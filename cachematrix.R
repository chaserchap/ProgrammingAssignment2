## Stores a matrix and caches information about the matrix, specifically the inverse.

## Assigns various parameters to the supplied matrix allowing the inverse to be stored to the matrix.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function () inv
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Uses solve() to find the inverse of the matrix, but first checks to see if the inverse has 
## already been computed, if it has it returns the previously found inverse.

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinverse(inv)
	inv
}
