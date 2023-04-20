
test_that(".warning throws a warning", {
    expect_warning(
        iSEEindex:::.warning("this is a warning"),
        "this is a warning"
    )
})

test_that(".stop throws an error", {
    expect_error(
        iSEEindex:::.stop("this is an error"),
        "this is an error"
    )
})
