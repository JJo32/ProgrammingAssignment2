
# First, we initialize two objects 'x' and 'matrixcache'. 'x' is the first argument of the
# function and defines our test matrix, of which we will cache and solve for the
# inverse. 'matrixchache' is set to NULL, and is to be utilized later on in the code.

# Next, we create and define the behavior of the functions nested in the parent
# enviornment (makeCacheMatrix). When run, 'setmatrix will replace the value of
# 'x'with the value of'y', which is undefined, and reset 'matrixcache' with a NULL value.
# 'getmatrix' will return the value of matrix 'x'. 'setmatrixcache' will replace the NULL value
# of 'matrixcache' with the value of the solved matrix. 'getmatrixcache' will
# retrieve the value of 'mateixcache'. Lastly, 'list' puts all of the nested
# functions into a list that can be accessed individually by name later on.


makeCacheMatrix <- function(x = matrix()) {
        matrixcache <- NULL 

        setmatrix <- function(y) {
                x <<- y
                matrixcache <<- NULL

        }
        getmatrix <- function() x
        setmatrixcache <- function(solved) matrixcache <<- solved
        getmatrixcache <- function() matrixcache

        list(setmatrix = setmatrix,
             getmatrix = getmatrix,
             setmatrixcache = setmatrixcache,
             getmatrixcache = getmatrixcache)


}


# We create a new function 'cacheSolve' because we need it to both store and
# retrieve information from our original function 'makeCacheMatrix'. It is
# initialized with a default argument 'x' as well as '...', which allows additional
# arguments to be passed through.

# First, we try to retrieve the solved matrix from the input of the function 'x'.
# by calling $getmatrixcache on 'x'. If the value of 'getmatrix' is not NULL, then the
# value of 'getmatrix' will be assigned to 'matrixcache' and the value of
# 'matrixcache' will be printed. If not, then the values of the unsolved matrix
# will be stored in a new variable named 'newmatrix'. We then 'solve' for
# 'newmatrix' and assign its value to 'matrixcache', and print it.

cacheSolve <- function(x, ...) {

        matrixcache <- x$getmatrixcache()

        if (!is.null(matrixcache)) {
                message( "getting cached data")
                return(matrixcache)
        }



                newmatrix <- x$getmatrix()
                matrixcache <- solve(newmatrix,...)
                x$setmatrixcache(matrixcache)
                return(matrixcache)


}
