# These two R functions allow us to cache the inverse of a matrix.

# The makeCacheMatrix creates a list of functions for matrix x.
# The makeCacheMatrix functions allows us to set and get the matrix x and its inverse i.
makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    
    set <- function(y) {                # function that allows us to (re)set the matrix and reset i to null,  
        x <<- y
        i <<- NULL
    }
    
    get <- function() {                 # function that returns matrix x
        x
    }
    
    setinverse <- function(inverse) {   # function that allows us to set the inverse matrix i
        i <<-inverse
    }
    
    getinverse <- function() {          # function that returns the inverse matrix i
        i
    }
    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
                                        #Naming the members of the list of functions
}


## The cacheSolve function returns a matrix that is the inverse of matrix x.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()                 # function to get the inverse of x 
        
        if(!is.null(i)){                    # if i has already been calculated and is not null
            message("getting cached data")  # i is returned from the cached data(<<-) and we exit the function
            return(i)
        }
        data <- x$get()                     # otherwise we use x$get() to get the matrix x
        i <- solve(data, ...)               # and use solve to calculate the inverse
        x$setinverse(i)                     # the inverse is set
        i                                   # and returned
}
