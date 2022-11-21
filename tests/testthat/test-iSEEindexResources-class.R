

test_that("show(iSEEindexResource) works", {

    out <- new("iSEEindexResource", uri = "https://zenodo.org/record/7304331/files/ReprocessedAllenData.rds?download=1")
    expect_output(show(out), "class: iSEEindexResource")

})

test_that("precache(iSEEindexResource) throws an error", {

    out <- new("iSEEindexResource", uri = "https://zenodo.org/record/7304331/files/ReprocessedAllenData.rds?download=1")
    expect_error(precache(out), "no 'precache' method defined for object of class")

})

# iSEEindexHttpsResource ----

test_that("show(iSEEindexHttpsResource) works", {

    out <- new("iSEEindexHttpsResource", uri = "https://zenodo.org/record/7304331/files/ReprocessedAllenData.rds?download=1")
    expect_output(show(out), "class: iSEEindexHttpsResource")

})

test_that("precache(iSEEindexHttpsResource) returns the original URI", {

    out <- new("iSEEindexHttpsResource", uri = "https://zenodo.org/record/7304331/files/ReprocessedAllenData.rds?download=1")
    expect_identical(precache(out), out@uri)

})
