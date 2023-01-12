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
#' @param metadata Named list of metadata. See individual resource classes for required and optional metadata.
#'
#' @return A `list` of [Panel-class] objects, representing an initial app state.
#'
#' @author Kevin Rue-Albrecht
#'
#' @rdname INTERNAL_load_initial
.load_initial <- function(bfc, dataset_id, config_id, metadata) {
    if (identical(config_id, .initial_default_choice)) {
        initial <- NULL
    } else {
        bfc_config_id <- paste0(dataset_id, "_", config_id)
        # TODO: refactor to a function that is also used by .load_sce
        bfc_result <- bfcquery(bfc, bfc_config_id, field = "rname", exact = TRUE)
        # nocov start
        if (nrow(bfc_result) == 0) {
            uri_object <- .metadata_to_object(metadata)
            script_path <- precache(uri_object, bfc, bfc_config_id)
        } else {
            script_path <- bfc[[bfc_result$rid]]
        }
        # nocov end
        env <- new.env()
        source(script_path, local = env)
        if (!exists("initial", where = env)) {
            stop("No object named 'initial' was found - this needs to be ",
                 "defined in the config script.")
        }
        initial <- get("initial", pos = env)
    }
    initial
}

#' Check Validity of Initial Configurations Metadata
#'
#' @param x `data.frame` of metadata.
#'
#' @return Invisible `NULL` if the metadata table is valid. Otherwise, throw an error.
#'
#' @author Kevin Rue-Albrecht
#'
#' @rdname INTERNAL_check_initial_table
#'
#' @examples
#' x <- data.frame(
#'   dataset_id = "dataset01",
#'   config_id = "dataset01_config01",
#'   label = "Initial configuration 01",
#'   uri = "https://example.com/dataset01_config01.R",
#'   description = "Initial configuration 01 for data set 01."
#' )
#' iSEEindex:::.check_initial_table(x)
.check_initial_table <- function(x) {
    # If x is NULL, it is valid without further checks.
    if (is.null(x)) {
        return(invisible(NULL))
    }
    # If x is not null it must have at least one row.
    if (identical(nrow(x), 0L)) {
        txt <- "If not NULL, initial configurations metadata must have at least one row."
        .stop(txt)
    }
    # Check that all required column names are present.
    required_colnames <- c(.initial_dataset_id, .initial_config_id,
        .initial_label, .initial_uri, .initial_description)
    for (column_name in required_colnames) {
        if (!column_name %in% colnames(x)) {
            txt <- sprintf("Required column '%s' missing in initial configurations metadata.", column_name)
            .stop(txt)
        }
    }
    # Check that config identifiers are unique
    which_dup <- duplicated(x[[.initial_config_id]])
    if (any(which_dup)) {
        first_dup <- which(which_dup)[1]
        txt <- sprintf("duplicate config_id: %s", x[[.initial_config_id]][first_dup])
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
