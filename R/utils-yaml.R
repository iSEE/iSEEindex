#' Import Resource Metadata from YAML
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
    # rbind into a single data.frame
    df <- do.call("rbind", list_of_df)
    df
}
