# library(testthat); source("test-iSEEindex.R")

# iSEEindex ----

test_that("iSEEindex works", {

    out <- iSEEindex(bfc)
    expect_s3_class(out, "shiny.appobj")

})
