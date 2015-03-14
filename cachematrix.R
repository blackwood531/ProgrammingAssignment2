## The functions in this script do two things:
##
## makeCacheMatrix creates a matrix wrapped in two getter/setter pairs
## - get returns the matrix
## - set updates the matrix
## - getCachedProperty returns the property, if any, that has been cached with the matrix
## - setCachedProperty sets the cached property
##
## cacheSolve computes the matrix' inverse and saves the result as the matrix' cached property
## 
## makeCacheMatrix also provides a function called getInverse that I added because
## I'm not thrilled with the interface in the problem statement.
## The reason I'm not thrilled is that the setCachedValue function
## can be called with any argument, by any code. The caller doesn't have to be
## cacheSolve, and nothing compels the value to be the matrix' inverse.
##
## If that happens, say as the result of a bug, the bad value could be used later
## as if it were the inverse
## That's why I called them getCachedProperty and setCachedProperty
## instead of getInverse and setInverse because the cached property could be anything
## - getInverse computes the matrix's inverse, if necessary, and caches it;
##       if the matrix was previously cached, getInverse returns it instead of computing it again
##       this has the advantage that it can be relied upon to return the matrix' inverse


## makeCacheMatrix returns a list of wrapper functions as described above
## It also saves the matrix passed as its argument. Since it saves the matrix in
## the same environment in which the wrapper functions are defined, they can work with
## the matrix and the cached property without clashing with anything else that's going on
## in the program

makeCacheMatrix <- function(x = matrix()) {
    cachedProperty <- NULL
    inverse <- NULL
    
    ## update the matrix
    ## clear the cached property and the inverse
    ## since there's no longer a reason to believe they are correct
    set <- function(otherMatrix) {
        x <<- otherMatrix
        cachedProperty <<- NULL
        inverse <<- NULL
    }
    
    ## return the matrix
    get <- function() {
        x
    }
    
    ## update the cached property (supposed to be the inverse, but could be anything)
    setCachedProperty <- function(prop) {
        cachedProperty <<- prop
    }
    
    # return the cached property
    getCachedProperty <- function() {
        cachedProperty
    }
    
    # return the inverse, from cache if possible
    getInverse <- function(...) {
        if( is.null(inverse) ) {
            inverse <<- solve(x, ...)
        } else {
            message("using cached inverse")
        }
        inverse
    }
    
    ## makeCacheMatrix's return value is a list of functions,
    ## defined in an environment that also includes the matrix,
    ## cachedProperty, and inverse
    list(
        set = set,
        get = get,
        getCachedProperty = getCachedProperty,
        setCachedProperty = setCachedProperty,
        getInverse = getInverse
    )
}

## As described above, cacheSolve returns the matrix' cached property if there is one
## If not, it computes 
cacheSolve <- function(x, ...) {
    inv <- x$getCachedProperty()
    if( is.null(inv) ) {
        inv <- solve(x, ...)
        x$setCachedProperty(inv)
    } else {
        message("using cached inverse")
    }
    inv
}
