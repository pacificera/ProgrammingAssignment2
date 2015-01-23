# Compute the inverse of a matrix using an efficient cacheing strategy so that intermediate results are not recalculated
#

# constructs a special matrix (represented as a list) that can cache its inverse
#
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y)
        {
                x <<- y
                i <<- NULL
        }
        
        get <- function() x # getter for the matrix
        setinverse <- function(inverse) i <<- inverse # set the inverse
        getinverse <- function() i # get the inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) # return the helper functions as a list
}


# Useing the helper functions of makeCacheMatrix, compute the inverse of a matrix
#
# This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
#`cacheSolve` should retrieve the inverse from the cache.
#
cacheSolve <- function(x, ...) {
        
        # return the cached inverse if it exists
        i <- x$getinverse()
        if(!is.null(i))
        {
                return(i)
        }
        
        # nothing in cache so compute the inverse, cache it, and return it
        data <- x$get()
        i <- solve(data, ...) # computes the inverse of a matrix
        x$setinverse(i)
        i
}
