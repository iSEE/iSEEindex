test_that(".datasets_available works", {
    
    dataset_fun <- function() {
        read.csv(system.file(package = "iSEEindex", "datasets.csv"))
    }
    
    out <- iSEEindex:::.datasets_available(dataset_fun)
    
    expect_s3_class(out, "data.frame")
    expect_identical(nrow(out), 1L)
})
