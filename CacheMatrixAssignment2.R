################################## REMARKS ############################################
# Output from the code are presented in README.md (even if not asked for this assignment!) 
# Because it makes the code easier to understand
# From a mathematical perspective, the simpliest square matrix is of order 2 (2 rows and 2 columns)
# If this matrix is given by: A=(a b ; c d) then the inverse A^(-1)=1/DET(A) * (d -b; -c a)
# With DET(A)= a*d - b*c, DET(A) must be different of zero! 
# So a matrix (1 1; 1 1) will create an error! don't try such a matrix with this specific code...
# For more details (and going beyond order 2), have a look at:
# http://www.mathwords.com/i/inverse_of_a_matrix.htm
# BUT in this Coursera assignment we assume the matrix as being always inversible!

############################# FIRST FUNCTION #####################################
# makeCacheMatrix function returns a list of functions
# It allows to store a numeric matrix and a cached value of the inverse of the matrix
# It includes the following functions:
# setMatrix      set the value of a matrix
# getMatrix      get the value of a matrix
# cacheInverse   get the cached value (inverse of the matrix)
# getInverse     get the cached value (inverse of the matrix)

# makeCacheMatrix function
makeCacheMatrix <- function(x = numeric()) {
        
        # contains the cached value or NULL if nothing is cached
        # So we set it initially to NULL
        cache <- NULL
        
        #  Matrix storage
        setMatrix <- function(newValue) {
                x <<- newValue
                # The matrix is assigned a new value so we flush the cache
                cache <<- NULL
        }
        
        # Shows the stored matrix
        getMatrix <- function() {
                x
        }
        
        # Cache the inverse matrix computed thanks to R function solve 
        cacheInverse <- function(solve) {
                cache <<- solve
        }
        
        # Get the cached value
        getInverse <- function() {
                cache
        }
        
        # Returns a list. Note that each named element of the list is a function
        list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}

#################################  SECOND FUNCTION #######################################
# The function below computes the inverse of the matrix created with makeCacheMatrix
cacheSolve <- function(y, ...) {
        # Get the cached value
        inverse <- y$getInverse()
        # In case a cached value exists -> return it!
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        # Otherwise get the matrix, computes the inverse matrix and save it in the cache!
        matrix <- y$getMatrix()
        inverse <- solve(matrix)
        y$cacheInverse(inverse)
        
        # Finally the inverse matrix is returned
        inverse
}
