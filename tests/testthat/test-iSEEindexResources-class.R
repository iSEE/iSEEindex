

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

    x <- new("iSEEindexHttpsResource", uri = "https://zenodo.org/record/7304331/files/ReprocessedAllenData_config_01.R?download=1")
    out <- precache(x, bfc, "DUMMY")
    expect_true(file.exists(out))

    bfcremove(bfc, names(out))
})

# iSEEindexLocalhostResource ----

test_that("show(iSEEindexLocalhostResource) works", {

    out <- new("iSEEindexLocalhostResource", uri = "localhost://path/to/file")
    expect_output(show(out), "class: iSEEindexLocalhostResource")

})

test_that("precache(iSEEindexLocalhostResource) workls", {
    
    tf <- tempfile()
    file.create(tf)
    
    x <- new("iSEEindexLocalhostResource", uri = sprintf("localhost://%s", tf))
    out <- precache(x, bfc, "DUMMY")
    expect_true(file.exists(out))

    bfcremove(bfc, names(out))
    
    unlink(tf)
})

# iSEEindexRcallResource ----

test_that("show(iSEEindexRcallResource) works", {

    out <- new("iSEEindexRcallResource", uri = "rcall://system.file(package='iSEEindex','ReprocessedAllenData_config_01.R')")
    expect_output(show(out), "class: iSEEindexRcallResource")

})

test_that("precache(iSEEindexRcallResource) workls", {
    
    x <- new("iSEEindexRcallResource", uri = "rcall://system.file(package='iSEEindex','ReprocessedAllenData_config_01.R')")
    out <- precache(x, bfc, "DUMMY")
    expect_true(file.exists(out))

    bfcremove(bfc, names(out))
})



# iSEEindexRcallResource ----

test_that("show(iSEEindexS3Resource) works", {

    x <- new("iSEEindexS3Resource", uri = "s3://bucket/file.R")
    expect_output(show(x), "class: iSEEindexS3Resource")

})
