## makeCacheMatrix function stores a matrix
## CacheSolve caches the inverse value of the matrix, stored in makeCaheMatrix

## The following function contains a list of functions - set, get, setsolve,and
## getsolve. setsolve and getsolve stores the cache value of solve() operation

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y){
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## The following function calculates the inverse of the matrix passed in above
## above function with solve(). It first checks if inverse is already calculated
## If so, it returns the cache value. If not, it calculates the inverse of the
## matrix and set the value of the inverse in the cache via the setsolve()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)){
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
