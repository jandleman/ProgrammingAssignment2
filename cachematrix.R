################################################################################
## These functions are used to efficiently compute the inverse of a matrix. 
## Since this computation is a fairly expensive operation, the inverse will be  
## cached the first time it is computed, so that it doesn't need to be 
## re-computed on successive requests for the matrix inverse.
##
## Note that the "solve" function used to invert the matrix stores as floating
## point numbers. When you multiply a matrix by its inverse, you should get the
## identity matrix back, but do to rounding errors, the "zero" cells actually
## show as very small floating point numbers (example: -4.440892e-16). If you
## apply the "round" function to the resulting product matrix, you will see a
## correct identity matrix. Here are a set of R commands that you can use to
## test this:
##
## m1 <- makeCacheMatrix(matrix(sample.int(25),5,5))
## m1$getMx()
## m2 = cacheSolve(m1)
## m2
## round(m1$getMx() %*% m2)
################################################################################

################################################################################
## makeCacheMatrix creates a special data structure (actually a list in R)
## which will store the source matrix along with a placeholder for the inverse 
## of the matrix along with getter and setter functions. 
##
## Inputs:
##
## inMX : square invertible matrix
##
## Outputs:
##
## list structure containing the following functons:
##    setMx  : store inMx input (original) matrix.
##    getMx  : get original matrix.
##    setInv : store inverted matrix.
##    getInv : get inverted matrix.
################################################################################
makeCacheMatrix <- function (inMx = matrix())
{
    myInv = NULL

    ## store the original matrix
    setMx <- function(inSetMx)
    {
        inMX  <-  inSetMx
        myInv <<- NULL
    }
    ## get original matrix
    getMx <- function()
    {
        inMx
    }
    ## store inverted matrix
    setInv <- function(inInv)
    {
        myInv <<- inInv
    }
    ## get inverted matrix
    getInv <- function()
    {
        myInv
    }
    ## return list of functions
    list(setMx  = setMx,
         getMx  = getMx,
         setInv = setInv,
         getInv = getInv)
}

################################################################################
## cacheSolve checks to see if the cacheMatrix structure created in the first
## function contains the matrix inverse. If it does, the inverse is returned.
## If it does not contain the inverse, then the inverse is computed, it is 
## stored in the cachMatrix data structure, and the inverse is then returned.
## In other words, this function always returns the inverted matrix, but is 
## slower the first time since the inverse must be computed. All later 
## invocations are faster because they only return the cached inverse.
##
## Inputs:
##
## inMx  : square invertible matrix
## ...   : these are optional paramters, which if passed, will also be passed
##         to the call of the "solve" function.
##
## Outputs:
##
## myInv : the source matrix, inMx, inverted. 
##
################################################################################
cacheSolve <- function(inCacheMx, ...)
{
    myInv <- inCacheMx$getInv()
    if(!is.null(myInv))
    {
        ## if matrix was already inverted, return the cached value
        message("getting cached data")
        return(myInv)
    }
    ## if matrix was not already inverted, compute the inverse, cache it,
    ## and return it to the caller.
    myMx  <- inCacheMx$getMx()
    myInv <- solve(myMx, ...)
    inCacheMx$setInv(myInv)
    myInv
}
