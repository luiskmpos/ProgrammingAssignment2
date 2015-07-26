## makeCacheMatrix is a function that stores a list of functions (set, get,setinv, getinv)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) { #changes the matrix stored as x
        x <<- y
        m <<- NULL
        }
    
    get <- function() x #returns the matrix stored in the main function (makeCacheMatrix)
    
    setinv <- function(solve) m <<- solve
    
    getinv <- function() m
    
    #the functions created above are stored in a list
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## cacheSolve is a function that returns the inverse if is stored in cache, but if it's
#not in cache, solves the inverse and returns it.

cacheSolve <- function(x, ...) {
    m <- x$getinv() #the getinv function is called from the list in makecachematrix
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()  #get function is called from the list in makecachematrix
    m <- solve(data, ...)
    x$setinv(m)
    m
}
