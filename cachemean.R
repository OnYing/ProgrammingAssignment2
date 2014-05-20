# The <<- operator refers to variables that already exist in parent context

# The input of makeVector() function is a vector
# An object of the makeVector() functions is a special vector which contains a list of 4 functions
makeVector <- function(x = numeric()){
    m <- NULL
    #When you create an object of makeVector() m is set to NULL
    set <- function(y){
        x <<- y
        m <<- NULL
        # if a vector has already been assigned to the object of makeVector()
        # the <<- here overwrites this vector and the m value in the parent frame
    }
    # Command to (re)set the object of makeVector() using y as a numeric vector
    get <- function() {
        x
    }
    # Command to the numeric vector of the object of makeVector()
    setmean <- function(mean) {
        m <<-mean
        # <<- overwrites the m value of e.g. NULL
    }
    # Command to set mean as m
    # Entering a value in the setmean() function will manually set mean as m
    getmean <- function() {
        m
    }
    # Command to return m
    list(set=set, get=get, setmean=setmean, getmean=getmean)
    # This commands names the list members
}

#The input of cachemean is an object of the makeVector function
cachemean <- function(x, ...){
    m <- x$getmean()
    # m is calculated as the getmean function of vector x
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    # in case m has been calculated before and m is returned (from cached data) and we exit the this function
    data <- x$get()
    m <- mean(data,...)
    x$setmean(m)
    m
    # Otherwise we use x$get() to get the data and calculate the mean and return m
}
