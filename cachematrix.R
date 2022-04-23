##To solve this problem use two functions called makeCacheMatrix and cacheSolve

makeCacheMatrix <- function( matt = matrix() ) {
j <- NULL #cache value at first contains nothing so it is Null
    set <- function( matrix ){
       matt <<- matr
         j <<- NULL
    }
getm <- function() matt
setInvs <- function(solve) j<<- solve #solving inversed matrix and setting inverse
getInverse <- function() j #getting inversed value
list(set = set, get = get,
    setInvs = setInvs,
    getInvs = getInvs)
}

cacheSolve <- function(x, ...) {
    matt <- x$getInvs()
    ##condition to check the existence of cached value
    if(!is.null(matt) ) {
            message("getting cach ed of invers matt")
            return(matt)
    }
    info <- x$getm()
    ##compute the inverse by using matrix multiplication
    matt <- solve(info) %*% info
    x$setInverse(matt)
    matt
}
##test the matrix and problem
matt <-makeCacheMatrix(matrix(c(5,4,1,3,6,8,2,7,0),ncol=3,nrow=3))
cacheSolve(matt)
x$getm()
x$getInvs()
