## The purpose of these code is to create a cacheable function to get the inverse of a matrix


##Create an object which have getters and setters to support cache
makeCacheMatrix <- function(x = matrix()) {

	inverse <- NULL
	set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
      get <- function() x
      setinverse <- function(inv) inverse <<- inv
      getinverse <- function() inverse
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Function to determine the inverse of a matrix. It checks if the inverse is cached.
##If so, it returns the value, else it calculate and store it.

cacheSolve <- function(x, ...) {
       ## Return a matrix that is the inverse of 'x'
	 inverse <- x$getinverse()
       if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse

}



