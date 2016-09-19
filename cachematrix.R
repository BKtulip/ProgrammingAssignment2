## A pair of functions that cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	## Initialize the inverse matrix
	m <- NULL
	
	## Set the matrix 
	set <- function(y){
		x <<- y
		m <<-NULL
	}
	
	## Get the matrix 
	get <- function() x
	
	## set the inverse of the matrix
	setInverse <- function(inverse) m <<- inverse
	
	## Get the inverse of the matrix
	getInverse <-function() m
	
	## Return 
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed),then the cachesolve should retrive the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        ## Return the inverse of 'x' if the inverse has already been calculated (and the matrix has not changed)
        if(!is.null(m)){
        	message("getting cached data")
        	return(m)
        }
        
        ## Get the matrix
        data <- x$get()
        ## Computes the inverse of the matrix
        m <- solve(data)
        ## Set the inverse of the matrix
        x$setInverse(m)
        ## Return
        m
}
