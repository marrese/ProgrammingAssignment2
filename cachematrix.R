makeCacheMatrix <- function(x = matrix()) {
#
# This function vectorizes 4 function as follows
# set - initialize special vector
# get - get input variable
# setinv - evaluate inverse matrix
# getinv - get inverse matrix from cache
# 
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
cacheSolve <- function(x, ...) {
#
# The function cacheSolve(x...) evaluate inverse matrix of x and returns it.
# If the inverted matrix is already evaluated returns the cache matrix
#  
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached inverse matrix") #is already in the cache
                return(m)
        }
        data <- x$get()

        m <- solve(data, ...) # evaluate inverse of data and put on m
        x$setinv(m) #set m in the cache
        m # returns m
}
#
#
#
#Call  the makeCacheMatrix() function and assign it's
#  return value ( a list of four functions) to a variable m
#  m is now a list of four functions
# m <- makeCacheMatrix()

#use v's set function to create a vector 
#  containing the numbers 20 through to 40
# m$set( matrix( c(0, 2, 2, 0 ), 2, 2))

#use m's get function to retrieve the vector created 
# m$get()

#pass the matrix m  to the cacheMatrix() function
#   the mean of the numeric vector 20:40 should be returned
# cacheSolve(m)

#pass the matrix m to the cacheSolve() function a second time
#  the inverse of matrix should be returned
#  also a message "getting cached inverse matrix" indicating that the inverse
#  is not being calculated this time but is being retrieved from the cached
#  value
# cacheSolve(m)

#test if the product of m and inv(m) is an identity matrix (result = TRUE)
# all.equal( diag(2), m$get() %*% cacheSolve(m))

#test on larger matrix
# m$set( matrix( rnorm( 1000000 ), 1000, 1000 ))
# m1=cacheSolve(m)
# m2=cacheSolve(m)
# all.equal( diag(1000), m$get() %*% cacheSolve(m))



