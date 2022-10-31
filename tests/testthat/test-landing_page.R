# library(testthat); source("test-landing_page.R")

test_that(".landing_page works", {
    out <- iSEEindex:::.landing_page(bfc, dataset_fun)
    expect_type(out, "closure")
})

test_that(".create_persistent_objects works", {
    df <- data.frame(a=1, b="a")

    out <- iSEEindex:::.create_persistent_objects(df)
    expect_identical(out$datasets_table, df)
})
