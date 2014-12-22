##cache the inverse of a matrixo

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(mean) m <<- solve(x)
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)


}


##This function computes the inverse of the special "matrix" returned by

cacheSolve <- function(x, ...) {

m <- x$getinverse()
         if(!is.null(m)) {
                 message("getting cached data")
                 return(m)
         }
         data <- x$get()
         z<-solve(data)
		m <-z
         x$setinverse(m)
         m

        }


