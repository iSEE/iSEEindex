#' Import Resource Metadata from YAML
#' 
#' `.list_to_dataframe()` converts a list of options for a set of configurations
#' into a `data.frame` where each row represents one configuration.
#' See Details.
#' 
#' @details
#' `.list_to_dataframe()` supports sets of configurations with non-overlapping
#' option names.
#' The `colnames` of the output `data.frame` will be the union of all the
#' options found across all the configurations in the input list.
#'
#' @param x A `list` of resource metadata.
#'
#' @return A `data.frame`
#'
#' @rdname INTERNAL_list_to_dataframe
#'
#' @examples
#' library(yaml)
#'
#' datasets_file <- system.file(package="iSEEindex", "example.yaml")
#' yaml_data <- read_yaml(system.file(package = "iSEEindex", "example.yaml"))
#'
#' # Data sets ----
#'
#' iSEEindex:::.list_to_dataframe(yaml_data$datasets)
#'
#' # Initial configurations ----
#'
#' iSEEindex:::.list_to_dataframe(yaml_data$initial)
.list_to_dataframe <- function(x) {
    # Convert list to a list of data.frames
    list_of_df <- lapply(x, as.data.frame)
    # Identify union of column names
    list_of_colnames <- lapply(list_of_df, colnames)
    unique_colnames <- unique(do.call("c", list_of_colnames))
    # Fill missing columns with NA
    list_of_df <- lapply(list_of_df, function(x) {
        x[, setdiff(unique_colnames, colnames(x))] <- NA
        x
    })
    # rbind into a single data.frame
    list_of_df <- lapply(list_of_df, function(x) 
        x[, intersect(colnames(x), unique_colnames), drop = FALSE]
    )
    df <- do.call("rbind", list_of_df)
    df
}
