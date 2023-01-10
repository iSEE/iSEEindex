#' Load Object and Coerce to SingleCellExperiment
#' 
#' @description 
#' `.load_sce()` loads the selected data set.
#' 
#' `.convert_to_sce()` coerces the selected data set to a [SingleCellExperiment-class] object.
#'
#' @param bfc A [BiocFileCache()] object.
#' @param id A data set identifier as a character scalar.
#' @param metadata Named list of metadata. See individual resource classes for required and optional metadata.
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
#' metadata <- list(uri="https://zenodo.org/record/7304331/files/ReprocessedAllenData.rds")
#' 
#' ## Usage ---
#' 
#' iSEEindex:::.load_sce(bfc, id, metadata)
#' 
.load_sce <- function(bfc, id, metadata) {
    bfc_result <- bfcquery(bfc, id, field = "rname", exact = TRUE)
    # nocov start
    if (nrow(bfc_result) == 0) {
        # TODO: refactor to a funtion that is also used by .load_initial
        uri_object <- .metadata_to_object(metadata)
        object_path <- precache(uri_object, bfc, id)
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

#' Convert Metadata to Class
#'
#' @param x Named list of metadata.
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
#' iSEEindex:::.metadata_to_object(list(uri="https://example.org/file.rds"))
#' iSEEindex:::.metadata_to_object(list(uri="localhost:///path/to/file.rds"))
#' iSEEindex:::.metadata_to_object(list(uri=
#'   "rcall://system.file(package='iSEEindex','ReprocessedAllenData_config_01.R')"
#' ))
#' iSEEindex:::.metadata_to_object(list(uri="s3://your-bucket/your-prefix/file.rds"))
#' iSEEindex:::.metadata_to_object(list(uri="s3://your-bucket/your-prefix/file.rds"))
.metadata_to_object <- function(x) {
    protocol <- urltools::url_parse(x[[.datasets_uri]])$scheme
    protocol_titled <- str_to_title(protocol)
    target_class <- sprintf("iSEEindex%sResource", protocol_titled)
    constructor.FUN <- try({
        get(target_class)
    }, silent = TRUE)
    if (is(constructor.FUN, "try-error")) {
        stop(
            "Failed to convert metadata to resource object. ",
            sprintf("Consider implementing the constructor function '%s()'.",
                    target_class)
            )
    }
    constructor.FUN(x)
}
