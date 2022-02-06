## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Initialize the two objects,x and m
makeCacheMatrix <- function(x = matrix()) {
        m <-NULL
        #Define the functions for objects of type makeCacheMatrix
        set <- function(y){
                x <<-y
                m <<-NULL
        }
        get <- function() x
        setSolve<- function(solve) m <<- solve
        getSolve<- function() m
        # Create a new object by returning a list()
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## Write a short comment describing this function
# Initialize with a single argument, x
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getSolve()
        # check to see if result is NULL. If not null, have a valid cached inverse matrix and can return it to the parent environment
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        #if result of !is.null(m) is FALSE, cacheSolve gets matrix from input object, 
        # calculates an inverse matrix, uses the setSolve() function on the input object
        # to set inverse matrix in the input object, then returns the value of
        # inverse matrix to the parent environment by printing the inverse matrix object.
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
        m
}


#Testing codes
# creating a simple matrix with 4 values of 2 columns x 2 rows
aMatrix <-makeCacheMatrix(matrix(1:4,ncol=2, nrow=2))
#retrive values of x
aMatrix$get()
#should be null because retrieve value of m
aMatrix$getSolve()
#reset value with new matrix
aMatrix$set(matrix(4:1,ncol=2, nrow=2))
#inverse matrix calculated for 4:1, not 1:4
cacheSolve(aMatrix)
# Retrieve directly now it's cached
aMatrix$getSolve()

