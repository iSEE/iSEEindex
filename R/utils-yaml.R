#' Import Resource Metadata from YAML
#' 
#' @description
#' `.datasets_to_dataframe()` converts a list of metadata for a set of data sets
#' into a `data.frame` where each row represents one data set.
#' See Details.
#' 
#' `.initial_to_dataframe()` converts a list of metadata for a set of initial
#' configurations into a `data.frame` where each row represents one initial
#' configuration.
#' 
#' @details
#' `.datasets_to_dataframe()` and `.initial_to_dataframe()` support sets of
#' options with non-overlapping option names.
#' The `colnames` of the output `data.frame` will be the union of all the
#' options found across all the configurations in the input list.
#' 
#' The metadata `datasets` restricts initial configurations to specific data sets.
#' Initial configurations missing the metadata `datasets` are made available to
#' all data sets.
#' As such, `.initial_to_dataframe()` replaces missing `datasets` metadata with the
#' full list of data set identifiers.
#'
#' @param x A `list` of resource metadata.
#' @param all_datasets_id Character vector of data set identifiers.
#'
#' @return A `data.frame`
#'
#' @rdname INTERNAL_metadata_to_dataframe
#'
#' @examples
#' library(yaml)
#'
#' datasets_file <- system.file(package="iSEEindex", "example.yaml")
#' yaml_data <- read_yaml(system.file(package = "iSEEindex", "example.yaml"))
#'
#' # Data sets ----
#'
#' datasets_df <- iSEEindex:::.datasets_to_dataframe(yaml_data$datasets)
#' datasets_df
#'
#' # Initial configurations ----
#'
#' iSEEindex:::.initial_to_dataframe(yaml_data$initial, datasets_df$id)
#' 
.datasets_to_dataframe <- function(x) {
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
    # reorder columns in matching order
    list_of_df <- lapply(list_of_df, function(x) 
        x[, intersect(colnames(x), unique_colnames), drop = FALSE]
    )
    # rbind into a single data.frame
    df <- do.call("rbind", list_of_df)
    df
}

#' @rdname INTERNAL_metadata_to_dataframe
.initial_to_dataframe <- function(x, all_datasets_id) {
    # Add missing data set metadata with all data set identifiers
    x <- lapply(x, function(x) {
        if (is.null(x[[.initial_datasets_id]])) {
            x[[.initial_datasets_id]] <- all_datasets_id
        }
        x
    })
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
    # reorder columns in matching order
    list_of_df <- lapply(list_of_df, function(x) 
        x[, intersect(colnames(x), unique_colnames), drop = FALSE]
    )
    # rbind into a single data.frame
    df <- do.call("rbind", list_of_df)
    df
}