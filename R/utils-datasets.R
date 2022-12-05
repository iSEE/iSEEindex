#' Load Object and Coerce to SingleCellExperiment
#' 
#' @description 
#' `.load_sce()` loads the selected data set.
#' 
#' `.convert_to_sce()` coerces the selected data set to a [SingleCellExperiment-class] object.
#'
#' @param bfc A [BiocFileCache()] object.
#' @param id A data set identifier as a character scalar.
#' @param uri A URI as a character scalar.
#'
#' @return
#' For `.load_sce()`, a [SingleCellExperiment()] object.
#' 
#' @author Kevin Rue-Albrecht
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
#' id <- "demo_load_sce"
#' uri <- "https://zenodo.org/record/7304331/files/ReprocessedAllenData.rds?download=1"
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
#' @author Kevin Rue-Albrecht
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
#' @author Kevin Rue-Albrecht
#' 
#' @importFrom methods new
#' @importFrom urltools url_parse
#' @importFrom stringr str_to_title
#'
#' @rdname INTERNAL_uri_to_object
#' 
#' @examples
#' iSEEindex:::.uri_to_object("https://example.org/file.rds")
#' iSEEindex:::.uri_to_object("localhost:///path/to/file.rds")
#' iSEEindex:::.uri_to_object(
#'   "rcall://system.file(package='iSEEindex',
#'                        ReprocessedAllenData_config_01.R')"
#' )
#' iSEEindex:::.uri_to_object("s3://your-bucket/your-prefix/file.rds")
#' iSEEindex:::.uri_to_object("s3://your-bucket/your-prefix/file.rds")
.uri_to_object <- function(uri) {
    protocol <- urltools::url_parse(uri)$scheme
    protocol_titled <- str_to_title(protocol)
    target_class <- sprintf("iSEEindex%sResource", protocol_titled)
    object <- try({
        new(target_class, uri = uri)
    })
    if (is(object, "try-error")) {
        stop(
            "Failed to convert URI to resource object. ",
            sprintf("Consider implementing the resource class '%s'.",
                    target_class)
            )
    } 
    
    object
}
