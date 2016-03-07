## The following functions enable the recovery of a cached inverse matrix and 
## the calculation of such a matrix. 

## makeCacheMatrix creates a list of functions that enable setting and getting
## the values of a matrix and its inverse. This matrices are cached to avoid
## repeating unnecessary calculations. 

makeCacheMatrix <- function(x = matrix()) {
    #Create an empty inverse matrix
    inverse_matrix <- NULL
    #set: function used to store the value of the original matrix
    #     and to clear the inverse_matrix.
    set <- function(y) {
        x <<- y
        inverse_matrix <<- NULL
    }
  
    #get: function used to get the value of the original matrix
    get <- function() x
    
    #set_inverse: function used to cache the inverse matrix
    set_inverse <- function (inverse) inverse_matrix <<-inverse
    
    #get_inverse: function used to recover the cached inverse matrix. 
    get_inverse <- function() inverse_matrix

    #Creating the output list with all of the necessary functions for caching the inverse. 
    list( set=set , get=get , 
          set_inverse=set_inverse , 
          get_inverse=get_inverse )
}


## cacheSolve recovers an existing cached inverse matrix or calculates the inverse 
## of the original cached matrix if it doesn't exist yet. 

cacheSolve <- function(x, ...) {
    #First recovers the value inverse matrix
    inv_mat <- x$get_inverse()
    #If this value is not NULL then returns the cached value
    if(!is.null(inv_mat)){
        message ("getting cached inverse matrix")
        return(inv_mat)
    }
    #Else calculates the inverse using the solve function. 
    org_mat <- x$get()
    inv_mat <- solve(org_mat, ...)
    x$set_inverse(inv_mat)
    inv_mat
}
