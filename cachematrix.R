## Put comments here that give an overall description of what your
## functions do

## The two functions given in this R code will be used to cache the inverse of a
## square invertible matrix. 

## Write a short comment describing this function
## The makeCacheMatrix function will create a matrix object that can cache its
##inverse.

makeCacheMatrix <- function(x = matrix()) {
  m_inverse <- NULL ## initialise the variable that will hold the cache of inverse
  set_matrix <- function(y) {
    x <<- y
    m_inverse <<- NULL
  }
  get_matrix <- function() x
  set_inverse <- function(solve) m_inverse <<- solve ## cache result of solve func
  get_inverse <- function() m_inverse ## func to retrieve inverse value from cache
  list(set_matrix = set_matrix, get_matrix = get_matrix,
       set_inverse = set_inverse, get_inverse = get_inverse)
}


## Write a short comment describing this function

## The cacheSolve function will compute the inverse of the matrix object returned
## by the makeCacheMatrix function defined above. If the inverse has already been
## computed, then this function will retrieve the cached value and return the same.

cacheSolve <- function(x, ...) {
  m_inverse <- x$get_inverse()
  if(!is.null(m_inverse))  { ## If checks whether cache exists
    message ("getting the matrix inverse from cache")
    ## Return a matrix that is the inverse of 'x' from cache
    return(m_inverse)
  } else { ## As no cache present, compute the inverse of the matrix
    calc_mat_inv <- x$get_matrix()
    m_inverse <- solve(calc_mat_inv) ## Function solve used to compute inverse
    x$set_inverse(m_inverse)
    ## Return a matrix that is the inverse of 'x'
    m_inverse
  }
}
