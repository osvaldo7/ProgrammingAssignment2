## Put comments here that give an overall description of what your
## functions do
## Esta funcion guarda en una varible fuera del ambiente de trabajo actual la matrix
## invertida, y define las funciones de set, get
##
## This function stored in a varible outside the current workplace the matrix
## Inverted , and defines the functions set, get

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
    set <- function(y) {
            x <<- y
           m <<- NULL
        }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
    setsolve = setsolve,
    getsolve = getsolve)

}


## Write a short comment describing this function
## Recibe un parametro de la clase makeCacheMatrix y utiliza la funcion
## getsolve para obtener la matrix invertida del cache definido en el otro ambiente
## Si no lo encuentra la genera la inversion con la funcion solve y la guarda con
## la instruccion setsolve
##
## The google translate:
## Receive a parameter of makeCacheMatrix class and uses the function
## getsolve to get the inverted matrix from cache defined in another environment
## If not found the its generates with the solve function and stores matrix with
## The instruction setsolve

cacheSolve <- function(x, ...) {
	m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
    ## Return a matrix that is the inverse of 'x'
}
