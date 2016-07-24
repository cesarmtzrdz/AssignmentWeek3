## This function will cache the matrix if it is already cached. 
## will save in another workspace or scope the matrix

makeCacheMatrix <- function(matrixx = matrix()) {
        
        #clean the variable
        inverseMatrix <- NULL
        
        setMatrix <- function(m) {
                #store matrixx to cache
                matrixx <<- m
                #clean cached inverse
                inverseMatrix <<- NULL
        }
        
        getMatrix <- function(){
                matrixx
        }
        
        #set the cache inverse
        setInverseMatrix <- function(inverseM) {
                inverseMatrix <<- inverseM
        }
        
        #return the matrix inverse that it is cached
        getInverseMatrix <- function(){ 
                inverseMatrix
        }
        
        #so it can work as functions..
        list(setInverseMatrix = setInverseMatrix, 
             getInverseMatrix = getInverseMatrix,
             setMatrix = setMatrix,
             getMatrix = getMatrix)

}


## Write a short comment describing this function

cacheSolve <- function(matrix, ...) {
        
        # get the cached inverse..if any
        inverse <- matrix$getInverseMatrix()
        
        # if inverse it is cached then return it
        if(!is.null(inverse)) {
                message("Retrieving cached inverse..")
                return(inverse)
        }
        # if this point is reached,then it means the inverse is not cached, so we must calculate ir ans cache it
        matrixx <- matrix$getMatrix()
        inverse <- solve(matrixx)
        matrix$setInverseMatrix(inverse)
        
        # return the inverse
        inverse
}
