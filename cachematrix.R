## makeCacheMatrix: creates a special matrix which can cache its inverse
## cacheSolve: lazy solving of matrix inverse
## testCacheSolve: some tests to check correctness, all 5 tests should return true

## This function creates a list which contains a matrix and can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y         # use <<- y is assigned to x which is defined in parent environment, with single <- x would be a local variable
        s <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) s <<- solve
    getSolve <- function() s
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## This function solves the equation x %*% y = I for y, where I is the identity matrix and x is a 
## special matrix created with makeCacheMatrix. The inverse is cached as an object in the special matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    s <- x$getSolve()
    if(!is.null(s)) {
        #message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setSolve(s)
    s
}

# test function, all 5 tests in the end should return true
testCacheSolve <- function(n = 1000) {
    if (n > 2000) stop("n should be smaller than 2000")
    
    set.seed(1)
    c <-replicate(n, rnorm(n)) 
    cCache <- makeCacheMatrix(c)
    
    cat("1) not cached:\n")
    t1 <- system.time(inverse1 <- cacheSolve(cCache))
    str(inverse1)
    
    cat("\n2) recompute (should be cached), user time should be less than previous and close to zero:\n")
    t2 <- system.time(inverse2 <- cacheSolve(cCache))
    str(inverse2)
    
    cat("\n3) get cache value directly:\n")
    t3 <- system.time(inverse3 <- cCache$getSolve())
    str(inverse3)
    
    cat("\nuser times - t1: ", t1[1], "  t2: ", t2[1], "  t3: ", t3[1])
    cat("\nt1 >= t2? ", t1[1] >= t2[1])
    cat("\nt2 close to 0? ", t2[1] < 1e-04)
    cat("\nsame result 1 versus 2? ", identical(inverse1, inverse2))
    cat("\nsame result 1 versus 3? ", identical(inverse1, inverse3))
    cat("\noriginal * inverse == identity? ", all.equal(c %*% inverse1, diag(n)))
}