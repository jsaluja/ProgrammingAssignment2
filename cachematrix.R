## Below two functions calculate and return the inverse of a matrix (assumed square)
## The functions store the inverse in cache, and check the cache first before computing inverse

## makeCacheMatrix returns a list of functions to get, and set, the input matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	##Initialize the inverse matrix to null
        i <- NULL
	##Assign the formal argument (input matrix) to object x in the environment
	##Initialize the inverse matrix to null for new input matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
	##return the input matrix
        get <- function() x
	##Assign the inverse matrix to object i in the environment
        setInv <- function(inverse) i <<- inverse
	##return the inverse matrix
        getInv <- function() i
	##return a list with 4 elements (functions)
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}



## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	##Invoke the getInv function from list, assign to i
        i <- x$getInv()
	##Check if inverse has already been calculated i.e, is not null , then return inverse from cache
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
	##fetch the matrix, calculate its inverse, set Environment variable i to inverse, and return inverse
        data <- x$get()
        i <- solve(data)
        x$setInv(i)
        i
}
