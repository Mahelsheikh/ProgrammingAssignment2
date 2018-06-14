## these functions make use of cache to save time and processing power in 
## reinverting the matrix by using cache as the makeCacheMatrix takes the value
## of a square matrix as such 
## mymatrix<-makeCacheMatrix <- (x = matrix(1:4,nrow = 2))
## as returns a list of 4 with x as an object in the parent environment under
##x$get which will be used to return the inverted matrix in case the inv object
## in the parent environment equals NULL or it was rest by a new matrix entrie.

## the makeCacheMatrix sets in to NULL then there are 4 functions 2 setters and 
## 2 getters.
## set which takes the y function and set it in the parent environment 
## as x and resent the object inv in the parent environment in case there was a
## value from a previous run.
##get which is a function to return the entered matrix under this
##setinvmatrix which is the function to invert x and return inv in the parent
## envrionment
##getinvmatrix is storing the object inv under this for returning to in later 
## in cacheSolve function.


makeCacheMatrix <- function(x = matrix()) {
        inv<- NULL
        set<-function(y = matirx()){
                x <<- y
                inv <<- NULL
        }#2
        get <- function() x 
        setinvmatrix<- function(solve) inv <<- solve
        getinvmatrix<- function() inv
        list( set=set, get=get,
              setinvmatrix = setinvmatrix,
              getinvmatrix = getinvmatrix)
}#1


## cacheSolve does two things checks for inv value under x$getinvmatrix() and 
## if inv= NULL it calculates the inverted matrix from x$get and return the
## value of inv

cacheSolve <- function(x,...) {
    inv <- x$getinvmatrix()
    if(!is.null(inv)) {
            message(" getting chached data")
            return(inv)
    }#2
    data<- x$get()
    inv <- solve(data,...)
    x$setinvmatrix(inv)
    inv
    ## Return a matrix that is the inverse of 'x'
}# 1
