# library(testthat); source("test-outputs.R")

# .render_datasets_table ----

test_that(".render_datasets_table works", {
    df <- data.frame(a=1, b="a")
    output <- new.env()
    pObjects <- new.env()
    rObjects <- new.env()

    out <- iSEEindex:::.render_datasets_table(output, pObjects, rObjects)

    expect_null(out)
    expect_named(output, "iSEEindex_INTERNAL_datasets_table")
})

# .render_markdown_overview ----

test_that(".render_markdown_overview works", {
    output <- new.env()
    pObjects <- new.env()
    rObjects <- new.env()

    out <- iSEEindex:::.render_markdown_overview(output, pObjects, rObjects)

    expect_null(out)
    expect_named(output, "iSEEindex_INTERNAL_markdown_overview")
})

# .render_markdown_overview ----

test_that(".render_markdown_overview works", {
    output <- new.env()
    pObjects <- new.env()
    rObjects <- new.env()

    out <- iSEEindex:::.render_initial_overview(output, pObjects, rObjects)

    expect_null(out)
    expect_named(output, "iSEEindex_INTERNAL_initial_overview")
})
