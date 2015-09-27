# ProgrammingAssignment2
#The first function, makeVector creates a special "vector", which is really a list containing a function to
#  1.set the value of the vector
#  2.get the value of the vector
#  3.set the value of the inverse
#  4.get the value of the inverse

makeVector <- function(x = numeric()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function() i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#The following function calculates the inverse of the special "vector" created with the above function

cacheinverse <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- inverse(data, ...)
        x$setinverse(i)
        i
}

