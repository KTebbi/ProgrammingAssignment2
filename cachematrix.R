makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

m <- matrix(2)

aMatrix <- makeCacheMatrix(m)

aMatrix$get() #2

aMatrix$getinverse() #NULL

cacheSolve(aMatrix) #0.5

aMatrix$getinverse() #0.5

m <- matrix(c(3, -7, 5, 2), 2, 2)
#       [,1] [,2]
# [1,]    3    5
# [2,]   -7    2

aMatrix$set(m)

cacheSolve(aMatrix)
#             [,1]        [,2]
# [1,] 0.04878049 -0.12195122
# [2,] 0.17073171  0.07317073

aMatrix$getinverse()
#             [,1]        [,2]
# [1,] 0.04878049 -0.12195122
# [2,] 0.17073171  0.07317073