# library(testthat); source("test-landing_page.R")

test_that(".landing_page works", {
    out <- iSEEindex:::.landing_page(bfc, dataset_fun, initial_fun)
    expect_type(out, "closure")
})

test_that(".create_persistent_objects works", {
    dataset_table <- data.frame(a=1, b="a")
    initial_table <- data.frame(a=1, b="a")

    out <- iSEEindex:::.create_persistent_objects(dataset_table, initial_table)
    expect_identical(out$datasets_table, dataset_table)
    expect_identical(out$initial_table, initial_table)
})
