##To solve this problem use two functions called makeCacheMatrix and cacheSolve
##The folowing matrix is used as an example:
## |5 3 2|
## |4 6 7|
## |1 8 0|
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
##The cacheSolve function should retrieve the inverse from the cache if the inverse has already been calculated and the matrix has not changed
cacheSolve <- function(x, ...) {
    j <- x$getInvs()
    ##condition to check the existence of cached value
    if(!is.null(j) ) {
            message("getting cach ed of invers matt")
            return(j)
    }
    info <- x$getm()
    ##compute the inverse by using matrix multiplication
    j <- solve(info) %*% info
    ##set new inversed matrix to our matrix
    x$setInverse(matt)
    matt
}
##test the matrix and problem
mat <-makeCacheMatrix(matrix(c(5,4,1,3,6,8,2,7,0),ncol=3,nrow=3))
cacheSolve(mat)
x$getm()
x$getInvs()
