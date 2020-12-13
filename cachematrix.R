makeCacheMatrix <- function(x = matrix()){
        m <- NULL
## This initializes the variable m so that it can be used later to store the inverse of the matrix.
        set <- function(y){
                x <<- y
                m <<- NULL
        }
## This sets the value of x in the parent (makeCacheMatrix) environment.
## The <<- operater ensures that y will be assigned to x in the parent environment so that the value of x remains in memory when cacheSolve is called. 
        get <- function() x
## This allows for the retrieval of x, the matrix passed to the function as a formal argument. 
        setmatrix <- function(solve) m <<- solve
## This sets the value of the inverse of the matrix variable (m) in the parent environment.
        getmatrix <- function() m
## This retrieves the value of the inverse of the matrix variable (m).
        list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
## This creates a list of the function names so that they can be called with the $ operator.
}

cacheSolve <- function(x, ...){
        m <- x$getmatrix()
## This calls the function getmatrix() passing x as the argument. 
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
## This checks if an inverse of an array is cached.
## If it is cached it returns it without calculating it again.
        ma <- x$get()
        m <- solve(ma, ...)
        x$setmatrix(m)
        m
## If it isn't cached, this takes the matrix(x), solves for the inverse, and returns the inverse.
        
}