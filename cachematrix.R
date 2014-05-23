# The following are two functions that are used to find the inverse of a matrix.
# If an inverse is calculated, it will be stored for possible future use.


# The makeCacheMatrix function creates a special "matrix" object
# that can cache its inverse
makeCacheMatrix <- function(m = matrix()) {
    matrix_inverse <- NULL
    
    # set_matrix stores the matrix
    set_matrix <- function(y) m <<- y
    
    # set_matrix_inverse stores the inverse matrix
    set_matrix_inverse <- function(y) matrix_inverse <<- y
    
    # get_matrix returns the original matrix
    get_matrix <- function() m
    
    # get_matrix_inverse returns the inverse matrix
    get_matrix_inverse <- function() matrix_inverse
    
    # the list of the various functions comprising makeCacheMatrix
    list(set_matrix = set_matrix, set_matrix_inverse = set_matrix_inverse,
    get_matrix = get_matrix, get_matrix_inverse = get_matrix_inverse)
}


# This function will calculate the inverse of the matrix in an object returned
# by makeCacheMatrix.  If the object returned by makeCacheMatrix doesn't
# have an inverse, then this fuction will calculate it and cache it.  Otherwise,
# it will simply return the cached inverse in the makeCacheMatrix object.
cacheSolve <- function(m, ...) {
    if (is.null(m$get_matrix_inverse())) {
        h = m$get_matrix()
        inverse = solve(h)
        m$set_matrix_inverse(inverse)
    }
    matrix_inverse <- m$get_matrix_inverse()
    matrix_inverse
}


