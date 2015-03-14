## The functions in this script do two things:
##
## makeCacheMatrix creates a matrix wrapped in two getter/setter pairs
## - get returns the matrix
## - set updates the matrix
## - getCachedProperty returns the property, if any, that has been cached with the matrix
## - setCachedProperty updates the cached property
##
## cacheSolve computes the matrix' inverse and saves the result as the cached property
## 
## makeCacheMatrix also provides a function called getInverse that I added because
## I'm not thrilled with the interface in the problem statement.
## The reason I'm not thrilled is that the setCachedValue function
## can be called by any code, with any argument. The caller doesn't have to be
## cacheSolve, and nothing compels the value to be the matrix' inverse.
## If that happens, say as the result of a bug, the bad value could be used later
## as if it were the inverse. That's why I called the second getter/setter pair
## getCachedProperty and setCachedProperty instead of getInverse and setInverse
##
## The fifth function is the one I added to the interface, to illustrate a
## different approach that ensures that the cached inverse is actually an inverse
## (if you'd like, you can ignore getInverse when evaluating this assignment)
## 
## - getInverse computes the matrix's inverse, if necessary, and caches it;
##       if the matrix was previously cached, getInverse returns it instead of computing it again
##       this has the advantage that it can be relied upon to return the matrix' inverse


###################################################################################
## makeCacheMatrix returns a list of wrapper functions as described above
## It also saves the matrix passed as its argument. Since it saves the matrix in
## the same environment in which the wrapper functions are defined, they can work with
## the matrix and the cached property without clashing with anything else that's going on
## in the program
###################################################################################
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
    
    ## return the cached property
    getCachedProperty <- function() {
        cachedProperty
    }
    
    ## return the inverse, from cache if possible
    ## (if you'd like, you can ignore getInverse when evaluating this assignment)
    getInverse <- function(...) {
        if( is.null(inverse) ) {
            inverse <<- solve(x, ...)
        } else {
            message("using cached inverse")
        }
        inverse
    }
    
    ## makeCacheMatrix's return value is a list of functions,
    ## defined in an environment that includes (and encapsulates)
    ## the matrix, cachedProperty, and inverse
    list(
        set = set,
        get = get,
        getCachedProperty = getCachedProperty,
        setCachedProperty = setCachedProperty,
        getInverse = getInverse
    )
}

###################################################################################
## As described above, cacheSolve returns the matrix' cached property if there is one
## If not, it computes the inverse and saves it in the cached property
###################################################################################
cacheSolve <- function(cx, ...) {
    inv <- cx$getCachedProperty()
    if( is.null(inv) ) {
        inv <- solve(cx$get(), ...)
        cx$setCachedProperty(inv)
    } else {
        message("using cached property, hoping it's the inverse")
    }
    inv
}

###################################################################################
## A little bit of unit testing.
## (if you'd like, you can ignore this section when evaluating this assignment)
## a handy way to test these implementations is to use rnorm to create the data
## for the matrix. An example is below
###################################################################################
testAssignment2 <- function(n) {
    
    ## create a test matrix
    testMatrix <- makeCacheMatrix(matrix(rnorm(n^2),n,n))
    
    message(paste("\nmultiplying the matrix and its inverse should return the", n, "by", n, "identity matrix"))
    print(round(testMatrix$get() %*% cacheSolve(testMatrix),1))
    
    message("\nthe second call should yield the same result, plus a message about using the cached property")
    print(round(testMatrix$get() %*% cacheSolve(testMatrix),1))
    
    message("\nchanging the matrix should invalidate the cache, so the result")
    message("should stay the same, but the message should disappear")
    testMatrix$set(matrix(rnorm(n^2),n,n))
    print(round(testMatrix$get() %*% cacheSolve(testMatrix),1))
    
    message("\ncalling yet again should brint the message back")
    print(round(testMatrix$get() %*% cacheSolve(testMatrix),1))
    
    message("\n#########################################################")
    message("\nhere's the same test, using the caching getInverse function I added to the interface")
    print(round(testMatrix$get() %*% testMatrix$getInverse(),1))
    
    message("\nagain, the result should be the same as the previous call, except for the message")
    print(round(testMatrix$get() %*% testMatrix$getInverse(),1))
    
    message("\n#########################################################")
    message("\nchanging the matrix should invalidate the cache, so the result")
    message("should stay the same, but the message should disappear")
    testMatrix$set(matrix(rnorm(n^2),n,n))
    print(round(testMatrix$get() %*% cacheSolve(testMatrix),1))
    
    message("\ncalling yet again should bring the message back")
    print(round(testMatrix$get() %*% cacheSolve(testMatrix),1))
    
    message("same re-test, but using getInverse instead of cacheSolve")
    print(round(testMatrix$get() %*% testMatrix$getInverse(),1))
    
    message("\ncalling yet again should bring the message back")
    print(round(testMatrix$get() %*% testMatrix$getInverse(),1))
    
    message("\n#########################################################")
    message("\nfinally, here's what I don't like about the proposed interface:")
    message("the cached property can be set to anything, causing a failure,")
    message("or worse, a difficult to detect incorrect result")
    idfake <- diag(n)
    idfake[1] = idfake[1] - 0.1
    fakeInverse <- solve(testMatrix$get(),idfake)
    testMatrix$setCachedProperty(fakeInverse)
    print(round(testMatrix$get() %*% cacheSolve(testMatrix),1))
        
}
