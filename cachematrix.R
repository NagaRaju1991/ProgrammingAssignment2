## The below function will calculate the inverse of the matrix and caches it.
## When user wants to execute the inverse function on the same matrix, it will
## return the previously calculated value from cache, instead of calculating it again.

## This below function creates a special type matrix object which is really a
## list, that contains the functions to
##	a) set the value of the matrix
##	b) get the value of the matrix
##	c) set the value of the inversed matrix
##	d) get the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
m<-NULL
set<-function(y){
x<<-y
m<<-NULL
}
get<-function()x
setinverse<-function(inverse)m<<-inverse
getinverse<-function()m
list(set = set, get = get,
setinverse = setinverse,
getinverse = getinverse)
}


## The below function will create the inverse of the special matrix created by above
## function. Firstly, this function will check whether the inverse of the matrix has been
## calculated or not. If yes, it will return the cached inversed matrix, skipping the
## re-computation. If not calculated, it will calculate using solve function and also caches
## the output value using setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		matrix_inversed<-x$getinverse()
		if(!is.null(matrix_inversed))
		{
		message("getting cached matrix.")
		return(matrix_inversed)
		}
		inverse_data<-x$get()
		matrix_inversed<-solve(inverse_data,...)
		x$setinverse(matrix_inversed)
		matrix_inversed
}
