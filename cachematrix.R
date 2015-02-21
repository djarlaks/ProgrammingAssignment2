## These functions give some benefit to caching the inverse of a matrix 
## rather than computing it repeatedly

## Returns matrix object which can cache inversed matrix

makeCacheMatrix <- function(x = matrix()) {
    solve <- NULL;
    get <- function() { x };
    set <- function(y) {
        x <<- y;
        solve <<- NULL;
    };
    getsolve <- function() { solve };
    setsolve <- function(s) { solve <<- s };
    list(get = get,
         set = set,
         getsolve = getsolve,
         setsolve = setsolve);
}


## Returns inversed matrix of cacheMatrix object using caching mechanism

cacheSolve <- function(x, ...) {
    s <- x$getsolve();
    if (!is.null(s)) {
        message("getting cached object");
        return(s);
    }
    matrix <- x$get();
    s <- solve(matrix);
    x$setsolve(s);
    s
}
