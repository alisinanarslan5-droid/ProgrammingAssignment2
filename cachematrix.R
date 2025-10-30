makeCacheMatrix <- function(mat = matrix()) {
    inv <- NULL
    set <- function(newMat) {
        mat <<- newMat
        inv <<- NULL  }
    get <- function() mat
    setInverse <- function(inverseMat) inv <<- inverseMat
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)}

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv}

