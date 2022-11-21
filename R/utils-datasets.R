#' Available Data Sets
#'
#' Import and process metadata for available data sets.
#'
#' @param FUN A function.
#'
#' @return
#' `.datasets_available()` returns a `data.frame` of metadata for available data sets.
#' 
#' @examples
#' ## Setup ----
#' 
#' dataset_fun <- function() {
#'     out <- read.csv(system.file(package = "iSEEindex", "datasets.csv"))
#' }
#' 
#' ## Usage ----
#' 
#' iSEEindex:::.datasets_available(dataset_fun)
#'
#' @rdname INTERNAL_datasets_available
.datasets_available <- function(FUN) {
    FUN()
}

#' Load Object and Coerce to SingleCellExperiment
#'
#' @param bfc A [BiocFileCache()] object.
#' @param uri A URI as a character scalar.
#'
#' @return
#' `.load_sce()` returns a [SingleCellExperiment()] object.
#'
#' @import SingleCellExperiment
#' @importFrom BiocFileCache bfcadd bfcquery
#'
#' @rdname INTERNAL_load_sce
#' @examples
#' ## Setup ----
#' 
#' library(BiocFileCache)
#' bfc <- BiocFileCache()
#' uri <- "https://zenodo.org/record/7186593/files/ReprocessedAllenData.rds?download=1"
#' 
#' ## Usage ---
#' 
#' iSEEindex:::.load_sce(bfc, uri)
#' 
.load_sce <- function(bfc, uri) {
    uri_object <- .uri_to_object(uri)
    bfc_rname <- download(uri_object)
    # TODO: Call .get1 method on object of class
    bfc_result <- bfcquery(bfc, bfc_rname, field = "rname", exact = TRUE)
    # nocov start
    if (nrow(bfc_result) == 0) {
        object_path <- bfcadd(bfc, bfc_rname)
    } else {
        object_path <- bfc[[bfc_result$rid]]
    }
    # nocov end
    object <- readRDS(object_path)
    object <- .convert_to_sce(object)
    object
}

#' @param x An object Coercible to SingleCellExperiment
#'
#' @return
#' For `.convert_to_sce()`, a [SingleCellExperiment()] object.
#'
#' @importFrom methods is as
#' @importFrom SummarizedExperiment SummarizedExperiment
#'
#' @rdname INTERNAL_load_sce
.convert_to_sce <- function(x) {
    if (!is(x, "SummarizedExperiment")) {
        x <- as(x, "SummarizedExperiment")
    }
    if (!is(x, "SingleCellExperiment")) {
        x <- as(x, "SingleCellExperiment")
    }
    x
}

.uri_to_object <- function(uri) {
    protocol <- gsub("(.+)://.+", "\\1", uri)
    protocol_titled <- stringr::str_to_title(protocol)
    target_class <- sprintf("iSEEindex%sResource", protocol_titled)
    object <- try({
        new(target_class, uri = uri)
    })
    if (is(object, "try-error")) {
        stop(
            "Failed to convert URI to resource object. ",
            sprintf("Consider implementing the resource class '%s'.", target_class)
            )
    } 
    
    object
}
