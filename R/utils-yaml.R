#' Import Resource Metadata from YAML
#'
#' @param file Path to a suitable YAML file.
#'
#' @return A `data.frame`
#'
#' @rdname INTERNAL_list_to_dataframe
#'
#' @examples
#' datasets_file <- system.file(package="iSEEindex", "datasets.yaml")
#' iSEEindex:::.list_to_dataframe(datasets_file, id.field = "id")
#'
#' config_file <- system.file(package="iSEEindex", "initial.yaml")
#' iSEEindex:::.list_to_dataframe(config_file, id.field = "config_id")
.list_to_dataframe <- function(x, id.field) {
    # Convert list to a list of data.frames
    list_of_df <- lapply(x, as.data.frame)
    # rbind into a single data.frame
    df <- do.call("rbind", list_of_df)
    # cbind the identifier column before all other columns
    df_id <- data.frame(id_placeholder = row.names(df))
    colnames(df_id) <- id.field
    res <- cbind(df_id, df)
    res
}
