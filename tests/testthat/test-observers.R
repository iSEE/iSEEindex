# library(testthat); source("test-observers.R")

# .create_observers ----

test_that(".create_observers works", {
    input <- new.env()
    pObjects <- new.env()
    rObjects <- new.env()

    out <- iSEEindex:::.create_observers(input, session = NULL, pObjects, rObjects)
    expect_null(out)

})

# .create_launch_observer ----

test_that(".create_launch_observers works", {
    input <- new.env()
    pObjects <- new.env()
    FUN <- function(SE, INITIAL) invisible(NULL)

    out <- iSEEindex:::.create_launch_observers(FUN, bfc, input, session = NULL, pObjects)
    expect_null(out)

})
