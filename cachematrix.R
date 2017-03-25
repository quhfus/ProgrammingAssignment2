## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
        set <- function(y) {
		if(dim(y)[1] == dim(y)[2]){
                    x <<- y
                    i <<- NULL
		} else {
		    message("The input is not a matrix. Please try again!")
		}
	}
	get <- function() x
        getInverse <- function() i
	setInverse <- function(inverse) i <<- inverse 
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getInverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data)
	x$setInverse(i)
	i
}
