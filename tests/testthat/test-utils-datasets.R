test_that(".load_sce works", {
    
    ## Setup ----

    bfc <- BiocFileCache::BiocFileCache()
    id <- "ID1"
    url <- "https://zenodo.org/record/7186593/files/ReprocessedAllenData.rds?download=1"
    
    ## Usage ---
    
    out <- iSEEindex:::.load_sce(bfc, id, url)
    
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

# .uri_to_object ----

test_that(".uri_to_object works for https uri", {
    
    out <- iSEEindex:::.uri_to_object("https://test.com")
    expect_s4_class(out, "iSEEindexHttpsResource")
    
})

test_that(".uri_to_object throws an error for undefined protocols", {
    
    expect_error(iSEEindex:::.uri_to_object("ftp://test.com"), "Failed to convert URI to resource object.")
    
})
