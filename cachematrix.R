## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL           #stores the cached inverse

    #set the matrix 
    set <- function(y) {  
        x <<- y           
        inv <<- NULL      #new matrix, so set inverse to NULL
    }

    #return the matrix
    get <- function() x   

    #set inverse in cache
    setinverse <- function(inverse) inv <<- inverse 

    #return cached inverse
    getinverse <- function() inv

    #return a list with the functions as members
    list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # check if inverse is already in cache
        # if already in cache, return cached inverse
        #else caclculate the inverse, store in cache 
        #and return the inverse 
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
