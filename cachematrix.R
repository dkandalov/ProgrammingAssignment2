# Functions below are for creating cached matrix object and calculating matrix inverse.
# E.g.
# > x = makeCacheMatrix(matrix(c(4,2,7,6),2,2))
# > x$get()
#       [,1] [,2]
#  [1,]    4    7
#  [2,]    2    6
#
# > cacheSolve(x)
#       [,1] [,2]
#  [1,]  0.6 -0.7
#  [2,] -0.2  0.4


# Creates cached matrix "object" (in terms of R it is a list with named values).
#
# Arguments:
#  m - matrix value (optional, defautls to empty matrix if not specified)
makeCacheMatrix <- function(m = matrix()) {
    inverse <- NULL
    get <- function() { m }
    set <- function(newM) {
        m <<- newM
        inverse <<- NULL
    }
    getInverse <- function() { inverse }
    setInverse <- function(m) { inverse <<- m }
    list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}


# Calculates inverse matrix from cached matrix "object".
#
# Arguments:
#  m - cached matrix
#  log - flag to turn logging on/off
cacheSolve <- function(m, log = FALSE, ...) {
    if (is.null(m$getInverse())) {
        if (log) message("calculating data")
        inverse <- solve(m$get())
        m$setInverse(inverse)
    } else {
      if (log) message("returning cached data")
    }
    m$getInverse()
}
