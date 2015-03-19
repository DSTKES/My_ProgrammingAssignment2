## These functions work together to create the inverse of a matrix.  makeCacheMatrix
## acts as a container for the cached inverse matrix and the functions needed to 
## get the cached value.  The cacheSolve function applies the functions created in
## makeCacheMatrix


## makeCacheMatrix creates a cache variable to store the inverse of a matrix and 
## creates four functions that are made available through a list.

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function(y) {    
                x <<- y         # replaces x value in parent environment with y value
                inv_x <<- NULL  # sets cache value inv_x to null in parent environment           
        }
        get <- function() x
        set_inv <- function(solve) inv_x <<- solve # passes solve value to cache in parent environment
        get_inv <- function() inv_x
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv) # created functions placed in list for calling

}


## cacheSolve uses the functions created in makeCacheMatrix to compute the inverse of
## a given matrix or use the cached calculation if it exists.  The function accepts the `x`
## argument in the form of a list created in the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## This section checks if cached value exists
        inv_x <- x$get_inv()      
        if(!is.null(inv_x)) {
                message("getting cached data")
                return(inv_x)
        }
        
        ## This section executes if the cached value does not exist
        
        data <- x$get()           # execute `get` function to pull the given matrix data
        inv_x <- solve(data, ...) # computes the matrix inverse and holds value in inv_x
        x$set_inv(inv_x)          # passes inv_x value to parent environment to hold in cache
        inv_x
}
