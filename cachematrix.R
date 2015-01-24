## Solve and cache inverse of square matrix
## C15JonathanKay; Doc: khaister

## Initializes funciton used to cache. Obtains raw matrix.

makeCacheMatrix <- function(x=matrix()) {
  J <- NULL
  G <- function() x
  setImatrix <- function(Imatrix) J <<- Imatrix
  getImatrix <- function() J
  
  # return a list of functions as an R object
  list(G=G, setImatrix=setImatrix, getImatrix=getImatrix)
}


## Checks to see that the matrix is nonsingular. If it is, returns value, caches, and exits.

cacheSolve <- function(x) {
  J <- x$getImatrix()
  if(!is.null(J)){
    message("Cached data found. Getting result... Done.")
    return(J)
  }
  else {
    message("No cached data found. Calculating inverse matrix...")
    data <- x$get() # obtains matrix from object x
    J <- solve(data) # finds inverse matrix
    x$setImatrix(J) # assigns resulting inverse matrix to object x
    return(J)
  }
}