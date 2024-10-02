test_that(".load_sce works", {

    ## Setup ----

    bfc <- BiocFileCache::BiocFileCache()
    id <- "ID1"
    metadata <- list(
        uri = "https://zenodo.org/record/7186593/files/ReprocessedAllenData.rds?download=1"
    )

    ## Usage ---

    out <- iSEEindex:::.load_sce(bfc, id, metadata, already_se_object = FALSE)

    expect_s4_class(out, "SummarizedExperiment")
})

# .convert_to_sce ----

test_that(".convert_to_sce works for ExpressionSet", {

    out <- iSEEindex:::.convert_to_sce(ExpressionSet())
    expect_s4_class(out, "SingleCellExperiment")

})

test_that(".convert_to_sce works for SummarizedExperiment", {

    out <- iSEEindex:::.convert_to_sce(SummarizedExperiment())
    expect_s4_class(out, "SingleCellExperiment")

})

# .metadata_to_object ----

test_that(".metadata_to_object works for https uri", {

    out <- iSEEindex:::.metadata_to_object(list(uri = "https://test.com"))
    expect_s4_class(out, "iSEEindexHttpsResource")

})

test_that(".metadata_to_object throws an error for undefined protocols", {

    expect_error(iSEEindex:::.metadata_to_object(list(uri = "ftp://test.com")), "No constructor function available for scheme 'ftp'.", fixed = TRUE)

})

# .check_datasets_table ----

test_that(".check_datasets_list works for valid metadata", {

    x <- list(list(
        id = "dataset01",
        title = "Data Set 01",
        uri = "https://example.com/dataset01.rds",
        description = "My first data set."
    ))

    out <- iSEEindex:::.check_datasets_list(x)
    expect_null(out)

})

test_that(".check_datasets_list throws an error for missing required metadata", {

    x <- list(list(
        id = "dataset01",
        title = "Data Set 01",
        uri = "https://example.com/dataset01.rds"
    ))

    expect_error(
        iSEEindex:::.check_datasets_list(x),
        "Required metadata 'description' missing in data set metadata"
    )

})

test_that(".check_datasets_list throws an error for zero item", {

    x <- list()

    expect_error(
        iSEEindex:::.check_datasets_list(x),
        "Data set metadata must have at least one item."
    )

})

test_that(".check_datasets_list throws an error when duplicate id is present", {

    x <- list(list(
        id = "dataset01",
        title = "Data Set 01",
        uri = "https://example.com/dataset01.rds",
        description = "My first data set."
    ))
    x <- append(x, x)

    expect_error(
        iSEEindex:::.check_datasets_list(x),
        "duplicate data set identifier: dataset01"
    )

})
