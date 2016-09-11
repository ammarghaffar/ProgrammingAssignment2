## Function makeCacheMatrix creates and returns a special matrix object 

## Function cacheSolve creats the inverse of the matrix and keeps it in cache
## and if cacheSolve is called again it returns the cache copy 
##and never calculates again


makeCacheMatrix <- function(x = matrix()) {

 	x_inver <- NULL
    	set <- function(y) {
        	x <<- y
       	x_inver <<- NULL
    	}
    	get <- function() x
    	setinverse <- function(inverse_x) x_inver <<- inverse_x
    	getinverse <- function() x_inver
    	list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)


}


## "cacheSolve" is a fucntion that returns the inverse of a special matrix object
## that is created with the makeCacheMatrix.
## and saves the inverse calculated in the cache. and next time if cache is availble
## retuns the cache data rather than recalulating

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
   	if (!is.null(m)) {
      	message("getting cached data")
       	return(m)
    	} 
	else 
	{
		data <- x$get()
      	m <- solve(data, ...)
        	x$setinverse(m)
		return(m)
   	}

}
