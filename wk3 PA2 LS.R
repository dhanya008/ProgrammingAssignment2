## caching the Inverse of a matrix
## Matrix inversion helps to cache the inverse of a matrix  rather than compute it repeatedly

## In the below functions created a special object that first stores teh matrix and then caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL

}
get <- function () x
setInverse <- function (inverse) inv <<- inverse 
getInverse <- function () inv
list (set = set,
      get = get,
      setInverse = setInverse
      getInverse = getInverse )
}        

## This function computes the inverse of teh special "matrix" created by 
## makeCacheMatrix above. If teh inverse has already been calculated ( and the 
## matrix has not changed), then it shoul retrieve teh inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null (inv)) {
                message ("getting cached data")
                return ( inv)
   }
        mat <- x$get()
        inv <- solve (mat , ....)
        x$setInverse (inv)
        inv
                
}
