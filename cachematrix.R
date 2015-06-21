makeInversion <- function(x = matrix()) { ## makeInversion is a function that stores a list of functions.
  I <- NULL
  set <- function(y) { ##set is a function that changes the matrix stored in the main function. We don't need to use this function unless we want to change the vector.
    x <<- y ## x <<- y substitutes the vector x with y (the input) in the main function (makeInversion). If it was "x <- y" it would have substitute the vector x with y only in the set function.
    I <<- NULL ##I <<- NULL" restores to null the value of the matrix inversion I, because the old inversion of the old matrix is not needed anymore
  }
  get <- function() x  ## function that returns the matrix x stored in the main function
  setInversion <- function(matInv) I <<- matInv ## setInversion and getInversion are functions very similar to set and get. They don't perform the matrix inversion, they simply store the value of the input in a variable I into the main function makeInversion and return it
  getInversion <- function() I
  list(set=set, get=get, setInversion=setInversion, getInversion=getInversion) ##To store the 4 functions in the function makeInversion, we need the function list, so that when we assign makeInversion to an object, the object has all the 4 functions.
}

cacheSolve <- function(x, ...) {
  I <- x$getInversion() ##The first thing cacheSolve does is to verify the value I, stored previously with getInversion, exists and is not NULL. If it exists in memory, it simply returns a message and the value I, that is supposed to be the matrix inversion, but not necessarily.
  if(!is.na(I)) {
    message("getting cached data") ##If it was the case, "return(I)" would have ended the function. So everything that follows this if() is a sort of else {}
    return(I)
  }
  data <- x$getInversion() ##data gets the vector stored with makeInversion.  
  I <- solve(data) ##I calculates the inverse of the matrix
  x$setInversion(I) ##x$setInversion(I) stores it in the object generated assigned with makeInversion
  I
}
