## The functions below will allow the user to calculate the inverse of a square matrix 
## They will reduce computation costs by providing the cached matrix inverse (when available) rather than computing it repeatedly

## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 
                if(!is.matrix(x)) stop("x must be a matrix")  ## ensure input is a matrix
                m <- NULL                                   
                set <- function(y) {                    ## set the value of the matrix
                        x <<- y                         ## search parent environments for an existing definition of 'x', if found redefine it otherwise assign in global environment
                        m <<- NULL                      ## search parent environments for an existing definition of 'm', if found redefine it otherwise assign in global environment
                }
                get <- function() x                     ## get the value of the matrix
                setsolve <- function(solve) m <<- solve ## assign the value of the inverse of the matrix 'm' from the parent environment to 'setsolve'
                getsolve<- function() m                 ## get the value of the inverse of the matrix
                list(set = set, get = get,              ## Create the special matrix which is really a list
                     setsolve = setsolve,
                     getsolve = getsolve)
        
}


## The cacheSolve function will compute inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cachesolve will retrieve the inverse from the cache. Otherwise the inverse will be calculated.

cacheSolve <- function(x, ...) {
        
        m <- x$getsolve()                              ## get the inverse of 'x' from special list if it has been calculated
        if(!is.null(m)) {                              ## If the inverse of 'x' if it has been calculated ...
                message("getting cached data")         ## notify user if the data is retrived from cache
                return(m)                              ## return the cached value of the inverse of 'x'
        }
        data <- x$get()                                ## get cached matrix 'x' from special list
        m <- solve(data, ...)                          ## calculate the inverse of cached matrix 'x' and assign it to 'm'
        x$setsolve(m)                                  ## set solve in the special list
        m                                              ## Return a matrix that is the inverse of 'x'
}
