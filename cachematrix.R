
# Write the following functions
# 1. makeCacheMatrix: This function creates a special "matrix" object that can
## cache its inverse

### First input a square matrix into the makeCacheMatrix function 

makeCacheMatrix <- function(x = matrix(), ...) {
        inv <- NULL

        get <- function() {x}

        setInvM <- function(invM) {inv <<- invM}

        getInvM <- function() {inv}

        list(get = get,
             setInvM = setInvM,
             getInvM = getInvM)
}


# 2. cacheSolve: This function computes the invese of the special "matrix"
## returned by makeCacheMatrix above. In the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

### cacheSolve function will first chech whether the inverse has been calculated or not
### if so it prints the inverse
### if not it calculates the inverse

cacheSolve <- function(x, ...) {

        inv <- x$getInvM()

        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

        data <- x$get()

        inv <- solve(data)

        x$setInvM(inv)

        print(inv)
}

