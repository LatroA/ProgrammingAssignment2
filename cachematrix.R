## Programming assignment 2 functions

## makeCacheMatrix
## Creates a vector with the following functions:
## 1.  set the value of a matrix (set)
## 2.  get the value of a matrix (get)
## 3.  set the value of the matrix inverse (setinverse)
## 4.  get the vlaue of the matrix inverse (getinverse)

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
      x <<- y
      im <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) im <<- inverse
  getinverse <- function() im
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve
## Checks to see if the matrix inverse has already been calculated (for an unchanged matrix)
## if so, returns value
## if not, calculates and returns value

cacheSolve <- function(x, ...) {
    im <- x$getinverse()
    if(!is.null(im))
    {
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    im = solve(data)
    x$setinverse(im)
    im
}


