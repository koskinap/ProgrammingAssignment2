## The makeCacheMatrix function creates a "special matrix" object,
## which is a list of four functions performing operations on a matrix,
## acting as a cache memory
## This function should be called every time we look for the inverse of a
## new matrix, to make sure that the matrix in cache has not changed

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
## otherwise it is calculated calling solve(), and stored in cache via setinv()

cacheSolve <- function(x, ...) {
     
        inv <- x$getinv()

        if (!is.null(inv)){
                message("Returning cached inverse")
        }
        else{
                message("Calculating the inverse")
                data <- x$get()
                inv <- solve(data, ...)
                x$setinv(inv)
        }
        ## Return the Inverse
        inv
                
                
}
