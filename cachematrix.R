## The makeCacheMatrix function creates a "special vector",
## which is a list with four functions performing operations regarding a matrix

## 1. set, sets the matrix in cache equal to that given
## 2. get, returns the matrix currenty in cache
## 3. setinv, sets the inverse of the matrix in cache, produced by solve()
## 4. getinv, returns the inverse of x from cache to avoid costly calculations

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        
        setinv <- function(solved) inv <<- solved
        
        getinv <- function () inv
        
        list( set = set ,
              get = get,
              setinv = setinv, 
              getinv = getinv)
}


## The "cacheSolve" function returns the inverse of a matrix.
## If the inverse of the matrix given,has already been calculated
## and stored in cache,it is recalled to avoid costly calculations,
## otherwise it is calculated calling solve()

cacheSolve <- function(x, ...) {
     
        inv <- x$getinv()
        
        if (!is.null(inv)){
                message("Returning cached inverse")
                inv
        }
        
        message("Calculating the inverse")
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
                
                
}
