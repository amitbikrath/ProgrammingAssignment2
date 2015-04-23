##cache the inverse of a matrixo

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
## setting cache as null as nothing is cached
m <- NULL
 ## store the matrix
        set <- function(y) {
                x <<- y
                ## new matrix assigned hence previous cache set to null
                m <<- NULL
        }
        ## return the matrix
        get <- function() x
        
        ## caching the inverse
        setinverse <- function(mean) m <<- solve(x)
        ## returning the cached value i.e. cached inverse matrix
        getinverse <- function() m
        ## return the list with each element named after their respective functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)


}


##This function computes the inverse of the special "matrix" returned by

cacheSolve <- function(x, ...) {

m <- x$getinverse()
        ## get the cached value and check whether it is null or not if not then return the cached value
         if(!is.null(m)) {
                 message("getting cached data")
                 return(m)
         }
         ##if the cache is null then get the matrix..
         data <- x$get()
         ##..and calculate the inverse
         z<-solve(data)
		m <-z
         ## finally return the calculated inverse
         x$setinverse(m)
         m

        }


