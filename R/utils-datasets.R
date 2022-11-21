#' Load Object and Coerce to SingleCellExperiment
#'
#' @param bfc A [BiocFileCache()] object.
#' @param id A data set identifier as a character scalar.
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
#' bfc <- BiocFileCache(tempdir())
#' id <- "ID0"
#' uri <- "https://zenodo.org/record/7186593/files/ReprocessedAllenData.rds?download=1"
#' 
#' ## Usage ---
#' 
#' iSEEindex:::.load_sce(bfc, id, uri)
#' 
.load_sce <- function(bfc, id, uri) {
    bfc_result <- bfcquery(bfc, id, field = "rname", exact = TRUE)
    # nocov start
    if (nrow(bfc_result) == 0) {
        # TODO: refactor to a funtion that is also used by .load_initial
        uri_object <- .uri_to_object(uri)
        bfc_fpath <- precache(uri_object)
        object_path <- bfcadd(x = bfc, rname = id, fpath = bfc_fpath)
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

#' Convert URI to Class
#'
#' @param uri URI to a resource.
#'
#' @return An object of a class that matches the URI protocol.
#' 
#' @importFrom methods new
#' @importFrom stringr str_to_title
#'
#' @rdname INTERNAL_uri_to_object
.uri_to_object <- function(uri) {
    protocol <- gsub("(.+)://.+", "\\1", uri)
    protocol_titled <- str_to_title(protocol)
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
