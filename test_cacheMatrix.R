# Test cases for cacheMatrix functions

source("cachematrix.R")

# first test to ensure inverse created
test_that("matrix inverse is solved", {
    
    #fixture
    input  <- matrix(c(1,1,4,0,3,1,4,4,0), c(3,3))
    expected <- solve(input)
    
    #execute
    cache <- makeCacheMatrix(input);
    actual <- cacheSolve(cache);
    
    #assertions
    expect_that(expected, equals(actual))
})

# first test uses cached colution
test_that("matrix inverse solution is cached", {
    
    #fixture
    input  <- matrix(c(1,1,4,0,3,1,4,4,0), c(3,3))
    expected <- solve(input)
    
    #execute
    cache <- makeCacheMatrix(input);
    actual <- cacheSolve(cache);
    actualCached <- cacheSolve(cache);
    
    #assertions
    expect_that(expected, equals(actualCached))
})

# final test shows weakness of this function:
# cached values are not invalidated when the matrix changes
test_that("cached inverse remains when matrix changed", {
    
    #fixture
    input  <- matrix(c(1,1,4,0,3,1,4,4,0), c(3,3))
    expected <- solve(input)
    
    #execute
    cache <- makeCacheMatrix(input);
    actual <- cacheSolve(cache);
    
    #now change the matrix
    input[1,1] <- 2;
    newInverse <- solve(input)
    
    actualCached <- cacheSolve(cache);
    
    #assertions
    expect_that(expected, equals(actualCached))
    expect_that(newInverse, not(equals(actualCached)))
})
