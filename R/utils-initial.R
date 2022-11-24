#' Choices of Initial Configurations
#' 
#' `.initial_choices()` identifies choices of predefined initial app states
#' available for a given data set, and returns them as a named character vector
#' meant to populate a Shiny [selectizeInput()].
#'
#' @param id Data set identifier, as a character scalar.
#' @param available Table of metadata for available initial configurations, as a `data.frame`.
#' 
#' @details 
#' A default option is automatically prepended to the choices for all data sets.
#' That default option launches an initial state that includes one panel of
#' each type compatible with the information present in the data set.
#'
#' @return A named character vector of choices for initial states of the
#' [iSEE()] app for that data set.
#' 
#' @author Kevin Rue-Albrecht
#' 
#' @export
#' @rdname INTERNAL_initial_choices
.initial_choices <- function(id, available) {
    # x: URI of the data set
    choices <- c("Default" = .initial_default_choice)
    which_initial <- available[[.initial_dataset_id]] == id
    config_subset_table <- available[which_initial, , drop=FALSE]
    initial_choices <- config_subset_table$config_id
    names(initial_choices) <- config_subset_table[[.initial_label]]
    choices <- c(choices, initial_choices)
    choices
}

#' Load an Initial Application State
#' 
#' Loads the selected initial application state.
#' This can be a custom R script or the default initial state that creates
#' one panel of each class compatible with the contents of the data set.
#'
#' @param bfc A [BiocFileCache()] object.
#' @param dataset_id Character scalar. Identifier of the data set selected.
#' @param config_id Character scalar. Identifier of the configuration file to load.
#' @param uri Character scalar. URI of the configuration file to download, if needed.
#'
#' @return A `list` of [Panel-class] objects, representing an initial app state.
#' 
#' @author Kevin Rue-Albrecht
#'
#' @rdname INTERNAL_load_initial
.load_initial <- function(bfc, dataset_id, config_id, uri) {
    if (identical(config_id, .initial_default_choice)) {
        initial <- NULL
    } else {
        bfc_config_id <- paste0(dataset_id, "_", config_id)
        # TODO: refactor to a funtion that is also used by .load_sce
        bfc_result <- bfcquery(bfc, bfc_config_id, field = "rname", exact = TRUE)
        # nocov start
        if (nrow(bfc_result) == 0) {
            uri_object <- .uri_to_object(uri)
            bfc_fpath <- precache(uri_object)
            script_path <- bfcadd(x = bfc, rname = bfc_config_id, fpath = bfc_fpath)
        } else {
            script_path <- bfc[[bfc_result$rid]]
        }
        # nocov end
        source(script_path, local = TRUE)
        if (!exists("initial")) {
            stop("No object named 'initial' was found - this needs to be ",
                 "defined in the config script.")
        }
    }
    initial
}