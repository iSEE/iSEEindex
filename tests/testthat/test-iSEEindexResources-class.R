

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

# iSEEindexLocalhostResource ----

test_that("show(iSEEindexLocalhostResource) works", {

    out <- new("iSEEindexLocalhostResource", uri = "localhost://path/to/file")
    expect_output(show(out), "class: iSEEindexLocalhostResource")

})

test_that("precache(iSEEindexLocalhostResource) workls", {
    
    tf <- tempfile()
    file.create(tf)
    
    out <- new("iSEEindexLocalhostResource", uri = sprintf("localhost://%s", tf))
    expect_identical(precache(out), tf)
    
    unlink(tf)
})

# iSEEindexRcallResource ----

test_that("show(iSEEindexRcallResource) works", {

    out <- new("iSEEindexRcallResource", uri = "rcall://system.file(package='iSEEindex','ReprocessedAllenData_config_01.R')")
    expect_output(show(out), "class: iSEEindexRcallResource")

})

test_that("precache(iSEEindexRcallResource) workls", {
    
    out <- new("iSEEindexRcallResource", uri = "rcall://system.file(package='iSEEindex','ReprocessedAllenData_config_01.R')")
    expect_identical(precache(out), system.file(package='iSEEindex','ReprocessedAllenData_config_01.R'))
})
