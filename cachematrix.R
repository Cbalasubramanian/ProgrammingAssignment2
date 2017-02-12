## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

######################### R programming assignment - week 3 #################################


#### makeCacheMatrix is a function that takes a matrix as caches it's value and that of its inverse.
#### The function returns a list that contains its local functions set(),get(), set_inverse() and get_inverse() 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse_value) inv <<- inverse_value
        get_inverse <- function() inv
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}



##### Takes the list returned by the makeCacheMatrix function as input.
##### If the inverse for the cached matrix has already been calculated, this function returns it and exits.
##### If the inverse has not been calculated, this function gets the cached matrix, calculates its inverse using the solve() function and sets the inverse value
##### using the set_inverse function.




cacheSolve <- function(x, ...) {
        inv <- x$get_inverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$set_inverse(inv)
        inv
}



########### execution steps ###############

# test_matrix <- matrix(10:13,2,2)
# calling_function <- makeCacheMatrix(test_matrix)

# calling_function$get()
#     [,1] [,2]
#[1,]   10   12
#[2,]   11   13

# cacheSolve(calling_function)

#     [,1] [,2]
#[1,] -6.5    6
#[2,]  5.5   -5


#cacheSolve(calling_function)
#getting cached data
#    [,1] [,2]
#[1,] -6.5    6
#[2,]  5.5   -5

###################################################