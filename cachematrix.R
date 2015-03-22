## makeCacheMatrix creates a matrix object and caches its inverse
## cacheSolve finds inverse of a matrix
## If an inverse was found once, and the object of which we are attempting
## to find an inverse has not changed, then we will simply return the previously
## calculated inverse


## Creates a special "matrix" object that can cache its inverse

## makeCacheMatrix requires a matrix as argument
makeCacheMatrix <- function(x = matrix()) {
     
     ## local inverse matrix set to NULL
     inv <- NULL
     
     ## sets cached x value to y; sets cached inv to NULL
     set <- function(y){
          x <<- y
          inv <<- NULL
     }
     
     ## returns the cached x
     get <- function() x
     
     ## sets cached inverse to inv
     setinv <- function(inverse) inv <<- inverse
          
     ## returns cached inverse
     getinv <- function() inv
     
     ## Return a 4 element list with set, get, setinv, ang getinv
     list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matix has not changed),
## then cacheSolve should retrive the inverse from the cache

## Requires x; x is an object formed by makeCacheMatrix
cacheSolve <- function(x, ...) {
     
     ## Assigns inverse of x to local inv variable
     inv <- x$getinv()
     
     ## If inv is not NULL, then return cached inverse; stop further
     ## exectuion of the funcion
     if(!is.null(inv)){
          message("getting cached data")
          return(inv)
     }
     
     ## ---- We arrive here meaing; inv was null (i.e. was not cached;
     ##      calculated previously) ----
     
     ## Gets the matrix from x
     data <- x$get()
     
     ## Finds the inverse of data using solve
     inv <- solve(data, ...)
     
     ## Sets the inverse to x; in csae we decide to calculate the inverse
     ## of x again, it will be already there
     x$setinv(inv)
     
     ## Return inverse of matrix x
     inv
}
