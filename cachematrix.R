#' Functions 
#' - to create a special type of matrix and 
#' - cache matrix inverse calculation
#'
#' How to use:
#' 
#' x <- makeCacheMatrix(matrix(c(1, 4, 2, 5), nrow=2))
#' cacheSolve(x) # calculates the inverse here
#' cacheSolve(x) # this will return the cached result
#' 
#' Or
#' 
#' m_example <- matrix(c(1, 4, 2, 5), nrow=2)
#' cm <- makeCacheMatrix()
#' cm$set(m_example)
#' cacheSolve(cm)
#' cacheSolve(cm)


#' returns a list with four defined methods: set, get, set_inverse, get_inverse
#' 
#' @param x matrix
#' @return list with 4 methods: set, get, set_inverse, get_inverse 
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) m <<- inverse
    get_inverse <- function() m
    list(set = set, 
         get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


#' Inverses a matrix. 
#' If the cache is available, return it. If not, calculate the inverse and cache it
#' 
#' @param x object made from makeCacheMatrix()
#' @return inverse of the given matrix. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$get_inverse()
    if(!is.null(m)) {
        message("getting cached inverse matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...) # solve() will calculate matrix inverse in R
    x$set_inverse(m)
    m
}

