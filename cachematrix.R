# Cachematrix.R program

# How to use below functions
#
# Consider that matrice is you matrix you want to inverse
# First you have to create a new list of functions depending on the matrix
# Never call the list as the same name of your matrix, you will have an error
# on the second function otherwise
# It's because of the set function in the makeCacheMatrix function
# matrice2 <- makeCacheMatrix(matrice)

# In order to inverse your matrix, you just have to call
# the second function
# cacheSolve(matrice2)

# Example:

# Step 1 - Initilisation
# matrice <- rbind(c(1, 1/4), c(1/4, 1))
# matrice2 <- makeCacheMatrix(matrice)

# Step 2 - Inversion of the matrix
# Inv <- cacheSolve(matrice2)


# Function makeCacheMatrix
# This function creates a special "matrix" object that can cache its inverse
# You have to call this function before to call the second one
# This function will create a specific environment where the inverse will be cached

makeCacheMatrix <- function(x = matrix())
{
    
    inv <- NULL # Initialisation of the inverse of the matrix
    
    # Set: set the matrix
    # Get: get the matrix
    # Setinv: Set the inverse of the matrix
    # Getinv: get the inverse of the matrix
    
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }    
    
    get <- function() x
    setinv <- function(invmatrice) inv <<- invmatrice
    getinv <- function() inv
    
    
    list(set = set , get = get , setinv = setinv , getinv = getinv )
    
}



# Function cacheSolve
# This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x,...) 
{
    
    # Don't input the matrix as argument, but the output of the MakeCacheMatrix
    # We test that case
    
    if (class(x) == "matrix") { stop("The argument must be the output of the MakeCacheMatrix") }  
    
    
    # Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    
    # First case: the inverse has been already cached
    # We just collect the cache
    if (!is.null(inv))
    {
        message("getting cached inverted matrix")
        return (inv)  # End of the function
    }
    
    # Second case: the inverted matrix hasn't been computed yet
    # We have to compute it
    data <- x$get() # We catch the real matrix, which means with the class matrix
    
    # We test if data is as matrix. Otherwise, it means that we used the same name for the matrix
    # and the list in the output of the MakeCacheMatrix function
    if (class(data) != "matrix") { stop (paste0("Please don't use the same name for the matrix", 
                                                " and the list in return of the makeCacheMatrix")) }
    
    
    inv <- solve(data) # This function determines the inverse of inv
    x$setinv(inv) # We cache the inverse
    return(inv)
}


