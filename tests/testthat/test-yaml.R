test_that(".datasets_to_dataframe works", {
    
    x <- list(
        list(
            id = "dataset01",
            title = "Data Set 01",
            uri = "https://example.com/dataset01.rds",
            description = "My first data set."
        ),
        list(
            id = "dataset02",
            title = "Data Set 02",
            uri = "https://example.com/dataset02.rds",
            description = "My second data set."
        )
    )
    
    out <- iSEEindex:::.datasets_to_dataframe(x)
    
    expect_s3_class(out, "data.frame")
    expect_identical(nrow(out), 2L)
    expect_identical(colnames(out), c("id", "title", "uri", "description"))
    
})

test_that(".initial_to_dataframe works", {
    
    x <- list(
        list(
            id = "config01",
            datasets = "dataset01",
            title = "Initial configuration 01",
            uri = "https://example.com/dataset01_config01.R",
            description = "My first initial configuration."
        ),
        list(
            id = "config02",
            datasets = "dataset01",
            title = "Initial configuration 02",
            uri = "s3://example.com/dataset01_config02.R",
            description = "My second initial configuration.",
            region = "eu-west-2"
        )
    )
    
    out <- iSEEindex:::.initial_to_dataframe(x)
    
    expect_s3_class(out, "data.frame")
    expect_identical(nrow(out), 2L)
    expect_identical(colnames(out), c("id", "datasets", "title", "uri", "description", "region"))
    expect_identical(out[1, "region", drop = TRUE], NA_character_)
    
})
