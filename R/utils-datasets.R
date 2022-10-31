#' Available Data Sets
#'
#' Import metadata for available data sets.
#'
#' @param FUN A function .
#'
#' @return
#' `.datasets_available()` returns a `data.frame` of metadata for available data sets.
#' 
#' @examples 
#' ## Setup ----
#' 
#' dataset_fun <- function() {
#'     read.csv(system.file(package = "iSEEindex", "datasets.csv"))
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
