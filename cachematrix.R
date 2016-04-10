## Description: a set of 2 functions to save (& return) a matrix 
## and inverse in cache; 
## makeCacheMatrix returns the setter and getter functions
## cacheSolve returns the cached inverse or creates and caches it

## makeCacheMatrix returns a special vector of following functions
## set: set the matrix and store the value in cache 
## get: get the matrix
## setinverse: set the inverse of the matrix and store it in cache
## getinverse: get the inverse of the matrix

## input: creates an empty matrix; assignment is only through set functions

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<-inverse
	getinverse <- function() inv
	list(set = set, get = get,
	setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve takes a square matrix and checks if the inverse is
## stored in cache; if its not present creates, saves and returns the 
## inverse using the solve func in cache
## input is assumed to be invertible

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)){
		message("retrieving cached data")
		return(inv)
	}
	mat <- x$get()
	inv <- solve(mat)
	x$setinverse(inv)
	return(inv)
}
