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
#' @return An object of a class that matches the URI scheme.
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
    scheme <- urltools::url_parse(x[[.datasets_uri]])$scheme
    scheme_titled <- str_to_title(scheme)
    target_class <- sprintf("iSEEindex%sResource", scheme_titled)
    constructor.FUN <- try({
        get(target_class)
    }, silent = TRUE)
    if (is(constructor.FUN, "try-error")) {
        stop(
            sprintf("No constructor function available for scheme '%s'. ", scheme),
            sprintf("Consider implementing the constructor function '%s()'.", target_class)
            )
    }
    constructor.FUN(x)
}

#' Check Validity of Data Sets Metadata
#'
#' @param x `data.frame` of metadata.
#'
#' @return Invisible `NULL` if the metadata table is valid. Otherwise, throw an error.
#'
#' @author Kevin Rue-Albrecht
#'
#' @rdname INTERNAL_check_datasets_table
#'
#' @examples
#' x <- data.frame(
#'   id = "dataset01",
#'   label = "Data Set 01",
#'   uri = "https://example.com/dataset01.rds",
#'   description = "My first data set."
#' )
#' iSEEindex:::.check_datasets_table(x)
.check_datasets_table <- function(x) {
    # Check that all required column names are present.
    required_colnames <- c(.datasets_id, .datasets_label, .datasets_uri, .datasets_description)
    for (column_name in required_colnames) {
        if (!column_name %in% colnames(x)) {
            txt <- sprintf("Required column '%s' missing in data set metadata.", column_name)
            .stop(txt)
        }
    }
    # Check that the table has at least one row.
    if (identical(nrow(x), 0L)) {
        txt <- "Data set metadata must have at least one row."
        .stop(txt)
    }
    # https://github.com/iSEE/iSEEindex/issues/23
    if (.dataset_region %in% colnames(x)) {
        txt <- paste(
            "Per-resource AWS S3 regions are ignored pending resolution of https://github.com/paws-r/paws/issues/571.",
            "The app will use the default region (set in '~/.aws/config') instead.", sep = " "
        )
        .warning(txt)
    }
    invisible(NULL)
}
