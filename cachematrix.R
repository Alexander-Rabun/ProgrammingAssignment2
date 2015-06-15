# This program illustrates the use of R's caching ability to store the results of 
# the potentially time-consuming matrix inversion operation and retrieve these results 
# from the cache in lieu of repeating the computation.

# This function creates a list containing a function to:
# * set the value of the given matrix
# * get the value of this matrix
# * set the inverse of this matrix
# * get the inverse of this matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,  setsolve = setsolve, getsolve = getsolve)
}



# This function produces the inverse of the output from makeCacheMatrix. 
# First it checks to see if the inverse has already been calculated and cached.
# If so, it retrieves the inverse from the cache, skips the computation and, when
# run interactively, prints a message to that effect.
# Otherwise, it calculates the inverse of the matrix and sets the value of the 
# inverse in the cache using the setsolve function.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached inverse")
             		return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}


